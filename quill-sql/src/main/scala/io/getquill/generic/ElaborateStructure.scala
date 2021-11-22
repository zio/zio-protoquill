package io.getquill.generic


import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._
import io.getquill.parser.Lifter
import io.getquill.quat.Quat
import io.getquill.ast.{Map => AMap, _}
import io.getquill.metaprog.TypeExtensions
import io.getquill.metaprog.TypeExtensions._
import io.getquill.generic.DecodingType
import io.getquill.util.Format
import scala.annotation.tailrec

/**
 * Elaboration can be different whether we are encoding or decoding because we could have
 * decoders for certain things that we don't have encoders for and vice versa. That means
 * that the potentially something encoded as a value would be decoded as a case-class
 * or vice versa. Therefore, we need to differentiate whether elaboration is used on the
 * encoding side or the decoding side.
 */
enum ElaborationSide:
  case Encoding
  case Decoding

/**
 * Based on valueComputation and materializeQueryMeta from the old Quill
 * This was around to flesh-out details of the outermost AST of a query based on the fields of the
 * object T in Query[T] that the AST represents. For an example say we have something like this:
 * {{{
 * import io.getquill.ast.{ Ident => Id, Property => Prop, _ }
 * case class Person(name: String, age: Int)
 * query[Person].map(p => p) // or just query[Person]
 * }}}
 * That would turn into an AST that looks like this:
 * {{{
 * Map(EntityQuery("Person"), Id("p"), Id("p"))
 * }}}
 * This query needs to be turned into `SELECT p.name, p.age from Person p`, the problem is, before
 * Quats, Quill did not actually know how to expand `Ident("p")` into `SelectValue(p.name), SelectValue(p.age)`
 * (see SqlQuery.scala) since there was no type information. Therefore...
 * {{{
 * // We needed to convert something that looks like this:
 * query[Person].map(p => p) // i.e. Map(EntityQuery("Person"), Id("p"), Id("p"))
 *
 * // Into something that looks like this:
 * query[Person].map(p => p).map(p => (p.name, p.age))
 * // i.e. Map(Map(EntityQuery("Person"), Ident("p"), Ident("p")), Tuple(Prop(Id("p"),"name"), Prop(Id("p"),"age")))
 * }}}
 * This makes it easier to translate the above information into the finalized form
 * {{{
 * SELECT p.name, p.age FROM (SELECT p.* from Person p) AS p
 * }}}
 * (Note that redudant map would typically be flattened out since it is extraneous and the inner
 * SELECT would no longer be present)
 *
 * Some special provisions were made for fields inside optional objects:
 * {{{
 * case class Address(street: String, zip: Int)
 * case class Person(name: String, address: Option[Address])
 * // This:
 * query[Person]
 * // Would become this:
 * query[Person].map(p => (p.name, p.address.map(_.street), p.address.map(_.zip)))
 * }}}
 *
 * Now, since Quats were introduced into Quill since 3.6.0 (technically since 3.5.3), this step is not necessarily needed
 * for query expansion since `Ident("p")` is now understood to expand into it's corresponding SelectValue fields so for queries,
 * this stage could technically be elimiated. However, this logic is also useful for ActionMeta where we have
 * something like this:
 * {{{
 * case class Person(name: String, age: Int)
 * // This:
 * query[Person].insert(Person("Joe", 44))
 * // Needs to be converted into this:
 * query[Person].insert(_.name -> "Joe", _.age -> 44)
 * // Which is actually:
 * EntityQuery("Person").insert(
 *   Assignment(Id("x1"), Prop(Id("x1"), "name"), Constant("Joe")),
 *   Assignment(Id("x1"), Prop(Id("x1"), "name"), Constant(44))
 * )
 * }}}
 * The fact that we know that Person expands into Prop(Id("p"),"name"), Prop(Id("p"),"age")) helps
 * us compute the necessary assignments in the `InsertUpdateMacro`.
 */
object ElaborateStructure {
  import io.getquill.generic.GenericDecoder

  private[getquill] def ofProduct[T: Type](side: ElaborationSide, baseName: String = "notused", udtBehavior: UdtBehavior = UdtBehavior.Leaf)(using Quotes) =
    base[T](Structure.Node(baseName), side, udtBehavior)

  /** Go through all possibilities that the element might be and collect their fields */
  def collectFields[Fields, Types](node: Structure, fieldsTup: Type[Fields], typesTup: Type[Types], side: ElaborationSide)(using Quotes): List[Structure] = {
    import quotes.reflect.{Term => QTerm, _}

    (fieldsTup, typesTup) match {
      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].isProduct =>
        base[tpe](node, side) :: collectFields(node, Type.of[fields], Type.of[types], side)
      case (_, '[EmptyTuple]) => Nil
      case _ => report.throwError("Cannot Derive Sum during Type Flattening of Expression:\n" + (fieldsTup, typesTup))
    }
  }

  @tailrec
  def flatten[Fields, Types](node: Structure, fieldsTup: Type[Fields], typesTup: Type[Types], side: ElaborationSide, accum: List[Structure] = List())(using Quotes): List[Structure] = {
    import quotes.reflect.{Term => QTerm, _}

    def constValue[T: Type]: String =
      TypeRepr.of[T] match {
        case ConstantType(IntConstant(value)) => value.toString
        case ConstantType(StringConstant(value)) => value.toString
        // Macro error
      }

    import io.getquill.metaprog.Extractors._

    (fieldsTup, typesTup) match {
      // TODO These calls are expensive
      // do this first '[field *: fields], then do '[Option[tpe] *: types] internally

      case ('[field *: fields], '[Option[firstInnerTpe] *: types]) =>
        // Option[firstInnerTpe] could be Option[tpe] or Option[Option[tpe]] etc... so we need to find the most inner type
        `Option[...[t]...]`.innerOrTopLevelT(Type.of[firstInnerTpe]) match
          case '[tpe] =>
            if (Type.of[tpe].isProduct)
              val childTerm = Structure.Node(Type.of[field].constValue, optional = true)
              //println(s"------ Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a product ----------")
              val baseTerm = base[tpe](childTerm, side)
              flatten(node, Type.of[fields], Type.of[types], side, baseTerm +: accum)
            else
              val childTerm = Structure.Leaf(Type.of[field].constValue, optional = true)
              //println(s"------ Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a Leaf ----------")
              flatten(node, Type.of[fields], Type.of[types], side, childTerm +: accum)

      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].isProduct && Type.of[tpe].notOption  =>
        val childTerm = Structure.Node(Type.of[field].constValue)
        //println(s"------ Non-Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a product ----------")
        val baseTerm = base[tpe](childTerm, side)
        flatten(node, Type.of[fields], Type.of[types], side, baseTerm +: accum)

      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].notOption =>
        val childTerm = Structure.Leaf(Type.of[field].constValue)
        //println(s"------ Non-Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a Leaf ----------")
        flatten(node, Type.of[fields], Type.of[types], side, childTerm +: accum)

      case (_, '[EmptyTuple]) => accum.reverse

      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + (fieldsTup, typesTup))
    }
  }

  enum UdtBehavior:
    case Leaf
    case Derive

  /**
   * Expand the structure of base term into series of terms for a given type
   * e.g. for Term(x) wrap Person (case class Person(name: String, age: Int))
   * will be Term(name, Term(x, Branch), Leaf), Term(age, Term(x, Branch), Leaf)
   * Note that this could potentially be different if we are on the encoding or the
   * decoding side. For example, someone could create something like:
   * {{
   *   case class VerifiedName(val name: String) { ... }
   *   val decoding = MappedDecoding ...
   * }}
   * Since we only have decoders, we need to know that VerifiedName is an actual value
   * type as opposed to an embedded case class (since ProtoQuill does not require presence
   * of the 'Embedded' type). So we need to know that VerifiedName is going to be:
   * {{ Term(x, Leaf)) }}
   * as opposed what we would generically have thought:
   * {{ Term(name, Term(x, Branch), Leaf)) }}.
   * That means that we need to know whether to look for an encoder as opposed to a decoder
   * when trying to wrap this type.
   *
   */
  def base[T: Type](term: Structure, side: ElaborationSide, udtBehavior: UdtBehavior = UdtBehavior.Leaf)(using Quotes): Structure = {
    import quotes.reflect.{Term => QTerm, _}

    // for errors/warnings
    def encDecText = side match
      case ElaborationSide.Encoding => "encodeable"
      case ElaborationSide.Decoding => "decodeable"

    val isAutomaticLeaf = side match
      // Not sure why the UDT part is needed since it shuold always have a GenericEncoder/Decoder anyway
      case _ if (TypeRepr.of[T] <:< TypeRepr.of[io.getquill.Udt]) =>
        //println(s"------- TREATING UDT as Leaf ${Format.TypeOf[T]}")
        // If we are elaborating a UDT and are told to wrap normally, make sure that this is done
        // even if an encoder exists for the UDT. Otherwise, automatically treat the UDT as a Leaf entity
        // (since an encoder for it should have been derived by the macro that used UdtBehavior.Derive)
        udtBehavior match
          case UdtBehavior.Leaf => true
          case UdtBehavior.Derive => false
      case ElaborationSide.Encoding =>
        //println(s"------- ALREDY EXISTS Encoder for ${Format.TypeOf[T]}")
        Expr.summon[GenericEncoder[T, _, _]].isDefined
      case ElaborationSide.Decoding =>
        //println(s"------- ALREDY EXISTS Decoder for ${Format.TypeOf[T]}")
        Expr.summon[GenericDecoder[_, _, T, DecodingType.Specific]].isDefined

    // TODO Back here. Should have a input arg that asks whether elaboration is
    //      on the encoding or on the decoding side.
    // Expr.summon[GenericDecoder[_, T, DecodingType.Generic]] match
    //   case Some(v) => println(s"**** Find Generic Decoder for ${io.getquill.util.Format.TypeOf[T]}: ${v.show}")
    //   case None => println(s"**** Not found Generic Decoder for ${io.getquill.util.Format.TypeOf[T]}")

    // See if there is a generic encoder for it. Since there are no derived generic encoders,
    // (only Generic Decoders), it is a good way to tell if something is a value type or not.
    if (isAutomaticLeaf)
        term.asLeaf
      // Otherwise, summon the mirror and wrap the value
    else
      // if there is a decoder for the term, just return the term
      Expr.summon[Mirror.Of[T]] match
        case Some(ev) =>
          // Otherwise, recursively summon fields
          ev match
            case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
              val children = flatten(term, Type.of[elementLabels], Type.of[elementTypes], side)
              term.withChildren(children)
            // TODO Make sure you can summon a ColumnResolver if there is a SumMirror, otherwise this kind of decoding should be impossible
            case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
              // Find field infos (i.e. Term objects) for all potential types that this coproduct could be
              val alternatives = collectFields(term, Type.of[elementLabels], Type.of[elementTypes], side)
              // Then merge them together to get one term representing all of their fields types.
              // Say you have a coproduct Shape -> (Square(width), Rectangle(width,height), Circle(radius))
              // You would get Term(width, height, radius)
              alternatives.reduce((termA, termB) => termA.merge[T](termB))
            case _ =>
              report.throwError(s"Althought a mirror of the type ${Format.TypeOf[T]} can be summoned. It is not a sum-type, a product-type, or a ${encDecText} entity so it's fields cannot be understood in the structure-elaborator. It's mirror is ${Format.Expr(ev)}")
        case None =>
          report.throwError(s"A mirror of the type ${Format.TypeOf[T]} cannot be summoned. It is not a sum-type, a product-type, or a ${encDecText} entity so it's fields cannot be understood in the structure-elaborator.")
  }

  private def productized[T: Type](side: ElaborationSide, baseName: String = "x")(using Quotes): Ast = {
    val lifted = base[T](Structure.Node(baseName), side).toAst
    val insert =
      if (lifted.length == 1)
        lifted.head._1
      else {
        CaseClass(lifted.map((ast, name) => (name, ast)))
      }
    insert
  }

  // ofStaticAst
  /** ElaborateStructure the query AST in a static query **/
  def ontoAst[T](ast: Ast)(using Quotes, Type[T]): AMap = {
    // TODO AST transformations turn into the Select clause which is how I know the elaboration is Decoding
    //      however, this should really be a static argument passed into the method.
    val bodyAst = productized[T](ElaborationSide.Decoding)
    AMap(ast, Ident("x", Quat.Generic), bodyAst)
  }

  /** ElaborateStructure the query AST in a dynamic query **/
  def ontoDynamicAst[T](queryAst: Expr[Ast])(using Quotes, Type[T]): Expr[AMap] = {
    // TODO This method is used from the dynamic AST generation during the select clause which is how I know it's a decoding elaboration.
    //      however, this should really be a static argument passed into the method.
    val bodyAst = productized[T](ElaborationSide.Decoding)
    '{ AMap($queryAst, Ident("x", Quat.Generic), ${Lifter(bodyAst)}) }
  }

  def ofProductType[T: Type](baseName: String, side: ElaborationSide)(using Quotes): List[Ast] = {
    val expanded = base[T](Structure.Node(baseName), side)
    expanded.toAst.map(_._1)
  }

  def ofAribtraryType[T: Type](baseName: String, side: ElaborationSide)(using Quotes): Ast =
    productized(side, baseName)


  /**
   *
   *
   * Example:
   *   case class Person(name: String, age: Option[Int])
   *   val p = Person("Joe")
   *   lift(p)
   * Output:
   *   Quote(ast: CaseClass("name" -> ScalarTag(name), lifts: EagerLift(p.name, name))
   *
   * Example:
   *   case class Name(first: String, last: String)
   *   case class Person(name: Name)
   *   val p = Person(Name("Joe", "Bloggs"))
   *   lift(p)
   * Output:
   *   Quote(ast: CaseClass("name" -> CaseClass("first" -> ScalarTag(namefirst), "last" -> ScalarTag(last)),
   *         lifts: EagerLift(p.name.first, namefirst), EagerLift(p.name.last, namelast))
   *
   * Note for examples below:
   *  idA := "namefirst"
   *  idB := "namelast"
   *
   * Example:
   *   case class Name(first: String, last: String)
   *   case class Person(name: Option[Name])
   *   val p = Person(Some(Name("Joe", "Bloggs")))
   *   lift(p)
   * Output:
   *   Quote(ast:   CaseClass("name" -> OptionSome(CaseClass("first" -> ScalarTag(idA), "last" -> ScalarTag(idB))),
   *         lifts: EagerLift(p.name.map(_.first), namefirst), EagerLift(p.name.map(_.last), namelast))
   *
   * Alternatively, the case where it is:
   *   val p = Person(None) the AST and lifts remain the same, only they effectively become None for every value:
   *         lifts: EagerLift(None               , idA), EagerLift(None              , idB))
   *
   * Legend: x:a->b := Assignment(Ident("x"), a, b)
   */
  // TODO Should have specific tests for this function indepdendently
  // keep namefirst, namelast etc.... so that testability is easier due to determinism
  // re-key by the UIDs later
  def ofProductValue[T: Type](productValue: Expr[T], side: ElaborationSide)(using Quotes): TaggedLiftedCaseClass[Ast] = {
    val elaborated = ElaborateStructure.ofProduct[T](side)
    // create a nested AST for the Term nest with the expected scalar tags inside
    val (_, nestedAst) = productValueToAst(elaborated)
    // create the list of (label, lift) for the expanded entities
    val lifts = liftsOfProductValue(elaborated, productValue)
    // return nested AST keyed by the lifts list
    TaggedLiftedCaseClass(nestedAst, lifts)
  }

  def decomposedProductValue[T: Type](side: ElaborationSide)(using Quotes) = {
    val elaborated = ElaborateStructure.ofProduct[T](side)
    decomposedLiftsOfProductValue(elaborated)
  }

  def decomposedProductValueDetails[T: Type](side: ElaborationSide, udtBehavior: UdtBehavior)(using Quotes) =
    import quotes.reflect._

    def innerType(tpe: Type[_]) =
      tpe match
        case '[Option[t]] => Type.of[t]
        case _ => tpe

    def isOptional(tpe: Type[_]) =
      tpe match
        case '[Option[t]] => true
        case _ => false

    def summonElaboration[T: Type] =
      val elaboration = ElaborateStructure.ofProduct[T](side, udtBehavior = udtBehavior)
      if (elaboration.isLeaf)
        report.throwError(s"Error encoding UDT: ${Format.TypeOf[T]}. Elaboration detected no fields (i.e. was a leaf-type). This should not be possible.")
      elaboration

    val elaboration = summonElaboration[T]
    // If it is get the components
    val components =
      DeconstructElaboratedEntityLevels.withTerms[T](elaboration).map((term, getter, rawTpe) => {
        val tpe = innerType(rawTpe)
        val isOpt = isOptional(rawTpe)
        // Note, we can't look at term.optional for optionality because that one is only optional on the term level,
        // in reality the type might be optional on the parent level as well.
        // For example: case class Person(name: Option[Name], age: Int), case class Name(first: Option[String], last: String)
        // the `last` field in Name has to be treated as an optional (i.e. after DeconstructElaboratedEntityLevels elements Expr(p => p.name.first), Expr(p => p.name.last), etc... have been found)
        // so we have to treat p.name.last as an Option insead of a plain field.
        // For example if we want to convert fields of `p:Person` to a string we need to do p.name.last.map(v => v.toString) instead of
        // just tacking on .toString after the p.name.last expression since that would be p.name.last:Option[String].toString which
        // makes an invalid query. See the MapFlicer for an example of this.
        (term.name, isOpt, getter, tpe)
      })
    (components, elaboration.structType)
  end decomposedProductValueDetails

  private[getquill] def liftsOfProductValue[T: Type](elaboration: Structure, productValue: Expr[T])(using Quotes) =
    import quotes.reflect._
    // for t:T := Person(name: String, age: Int) it will be paths := List[Expr](t.name, t.age) (labels: List("name", "age"))
    // for t:T := Person(name: Name, age: Int), Name(first:String, last: String) it will be paths := List[Expr](t.name.first, t.name.last, t.age) (labels: List(namefirst, namelast, age))
    val labels = elaboration.paths
    val pathLambdas = DeconstructElaboratedEntityLevels[T](elaboration)
    val paths: List[Expr[_]] = pathLambdas.map { (exprPath, exprType) =>
      exprType match
        case '[t] =>
          if (TypeRepr.of[t] =:= TypeRepr.of[Any])
            lazy val showableExprPath = '{ (input: T) => ${ exprPath('input) } }
            report.warning(s"The following the expression was typed `Any`: ${Format.Expr(showableExprPath)}. Will likely not be able to summon an encoder for this (the actual type was: ${Format.TypeOf[T]} in ${Format.TypeRepr(showableExprPath.asTerm.tpe)})  (the other param was ${Format.TypeOf[T]}.")
          '{ ${exprPath(productValue)}.asInstanceOf[t] }
    }
    if (labels.length != pathLambdas.length)
      report.throwError(s"List of (${labels.length}) labels: ${labels} does not match list of (${paths.length}) paths that they represent: ${paths.map(Format.Expr(_))}")
    val outputs = labels.zip(paths)
    outputs.foreach { (label, exprPath) =>
      if (exprPath.asTerm.tpe =:= TypeRepr.of[Any])
        report.warning(s"`Any` value found for the path ${label} at the expression ${Format.Expr(exprPath)}. Will likely not be able to summon an encoder for this.")
    }
    outputs

  private[getquill] def decomposedLiftsOfProductValue[T: Type](elaboration: Structure)(using Quotes): List[(Expr[T] => Expr[_], Type[_])] =
    DeconstructElaboratedEntityLevels[T](elaboration)
  /**
   * Flatten the elaboration from 'node' into a completely flat product type
   * Technicallly don't need Type T but it's very useful to know for errors and it's an internal API so I'll keep it for now
   */
  private[getquill] def productValueToAst[T: Type](node: Structure /* i.e. the elaboration */)(using Quotes): (String, Ast) =
    def toAstRec(node: Structure, parentTerm: String, topLevel: Boolean = false): (String, Ast) =
      def notTopLevel(str: String) = if (topLevel) "" else str
      node match
        case Structure.Leaf(name, _) =>
          (name, ScalarTag(parentTerm + name))
        // On the top level, parent is "", and 1st parent in recursion is also ""
        case Structure.Node(name, false, list) =>
          val children = list.map(child => toAstRec(child, notTopLevel(parentTerm + name)))
          (name, CaseClass(children))
        // same logic for optionals
        case Structure.Node(name, true, list) =>
          val children = list.map(child => toAstRec(child, notTopLevel(parentTerm + name)))
          (name, OptionSome(CaseClass(children)))
        case _ =>
          quotes.reflect.report.throwError(s"Illegal generic schema: $node from type ${Type.of[T]}")
    toAstRec(node, "", true)

  extension [T](opt: Option[T])
    def getOrThrow(msg: String) = opt.getOrElse { throw new IllegalArgumentException(msg) }

  case class TaggedLiftedCaseClass[A <: Ast](caseClass: A, lifts: List[(String, Expr[_])]) {
    import java.util.UUID
    def uuid() = UUID.randomUUID.toString

    /** Replace keys of the tagged lifts with proper UUIDs */
    def reKeyWithUids(): TaggedLiftedCaseClass[A] = {
      def replaceKeys(newKeys: Map[String, String]): Ast =
        Transform(caseClass) {
          case ScalarTag(keyName) =>
            lazy val msg = s"Cannot find key: '${keyName}' in the list of replacements: ${newKeys}"
            ScalarTag(newKeys.get(keyName).getOrThrow(msg))
        }

      val oldAndNewKeys = lifts.map((key, expr) => (key, uuid(), expr))
      val keysToNewKeys = oldAndNewKeys.map((key, newKey, _) => (key, newKey)).toMap
      val newNewKeysToLifts = oldAndNewKeys.map((_, newKey, lift) => (newKey, lift))
      val newAst = replaceKeys(keysToNewKeys)
      TaggedLiftedCaseClass(newAst.asInstanceOf[A], newNewKeysToLifts)
    }
  }
}
