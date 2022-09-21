package io.getquill.generic

import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{Hidden, Visible}
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
import io.getquill.ast.External.Source
import zio.Chunk

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

  sealed trait TermType
  case object Leaf extends TermType
  case object Branch extends TermType

  // TODO Good use-case for zio-chunk
  case class TermPath(terms: List[Term]):
    def append(term: Term) = this.copy(terms = this.terms :+ term)
    def concat(path: TermPath) = this.copy(terms = this.terms ++ path.terms)
    def mkString(separator: String = "", dropFirst: Boolean = true) =
      (if (dropFirst) terms.drop(1) else terms).map(_.name).mkString(separator)
  object TermPath:
    def single(term: Term) = TermPath(List(term))

  // TODO Rename to Structure
  case class Term(name: String, typeType: TermType, children: List[Term] = List(), optional: Boolean = false) {
    def withChildren(children: List[Term]) = this.copy(children = children)
    def toAst = Term.toAstTop(this)
    def asLeaf = this.copy(typeType = Leaf, children = List())
    def paths =
      def pathsRecurse(node: Term, topLevel: Boolean = false): List[String] =
        def emptyIfTop(str: String) = if (topLevel) "" else str
        node match
          case Term(name, _, Nil, _) => List(name)
          case Term(name, _, childProps, _) =>
            childProps
              .flatMap(childTerm => pathsRecurse(childTerm))
              .map(childName => emptyIfTop(name) + childName)
      pathsRecurse(this, true)

    // Used by coproducts, merges all fields of a term with another if this is valid
    // Note that T is only needed for the error message. Maybe take it out once we store Types inside of Term
    def merge[T: Type](other: Term)(using quotes: Quotes) = {
      import quotes.reflect.{Term => TTerm, _}

      // Terms must both have the same name
      if (this.name != other.name)
        report.throwError(s"Cannot resolve coproducts because terms ${this} and ${other} have different names") // TODO Describe this as better error messages for users?

      if (this.optional != other.optional)
        report.throwError(s"Cannot resolve coproducts because one of the terms ${this} and ${other} is optional and the other is not")

      if (this.typeType != other.typeType)
        report.throwError(s"Cannot resolve coproducts because the terms ${this} and ${other} have different types (${this.typeType} and ${other.typeType} respectively)")

      import io.getquill.util.GroupByOps._
      // Given Shape -> (Square, Rectangle) the result will be:
      // Shape.x detected multiple kinds of values: List(Term(x,Branch,List(Term(width,Leaf,List(),false), Term(height,Leaf,List(),false)),false), Term(x,Branch,List(Term(radius,Leaf,List(),false)),false))
      // Need to merge these terms
      val orderedGroupBy = (this.children ++ other.children).groupByOrdered(_.name)
      // Validate the keys to make sure that they are all the same kind of thing. E.g. if you have a Shape coproduct
      // with a Square.height and a Rectagnle.height, both 'height' fields but be a Leaf (and also in the future will need to have the same data type)
      // TODO Need to add datatype to Term so we can also verify types are the same for the coproducts
      val newChildren =
        orderedGroupBy.map((term, values) => {
          val distinctValues = values.distinct
          if (distinctValues.length > 1)
            report.throwError(s"Invalid coproduct at: ${TypeRepr.of[T].widen.typeSymbol.name}.${term} detected multiple kinds of values: ${distinctValues}")

          // TODO Check if there are zero?
          distinctValues.head
        }).toList

      this.copy(children = newChildren)
    }

  }

  object Term:
    import io.getquill.ast._

    // Not sure if Branch Terms should have hidden properties
    def property(parent: Ast, name: String, termType: TermType) =
      Property.Opinionated(
        parent,
        name,
        Renameable.neutral,
        termType match {
          case Leaf   => Visible
          case Branch => Hidden
        }
      )

    def toAst(node: Term, parent: Ast): List[(Ast, String)] =
      node match {
        // If there are no children, property should not be hidden i.e. since it's not 'intermediate'
        // I.e. for the sake of SQL we set properties representing embedded objects to 'hidden'
        // so they don't show up in the SQL.
        case Term(name, tt, Nil, _) =>
          val output = List(property(parent, name, tt))
          // Add field name to the output: x.width -> (x.width, width)
          output.map(ast => (ast, name))

        case Term(name, tt, list, false) =>
          val output =
            list.flatMap(elem =>
              toAst(elem, property(parent, name, tt))
            )
          // Add field name to the output
          output.map((ast, childName) => (ast, name + childName))

        // Leaflnode in an option
        case Term(name, Leaf, list, true) =>
          val idV = Ident("v", Quat.Generic) // TODO Specific quat inference
          val output =
            for {
              elem <- list
              (newAst, subName) <- toAst(elem, idV)
            } yield (OptionMap(property(parent, name, Leaf), idV, newAst), subName)
          // Add field name to the output
          output.map((ast, childName) => (ast, name + childName))

        // Product node in a option
        case Term(name, Branch, list, true) =>
          val idV = Ident("v", Quat.Generic)
          val output =
            for {
              elem <- list
              (newAst, subName) <- toAst(elem, idV)
            } yield (OptionTableMap(property(parent, name, Branch), idV, newAst), subName)

          // Add field name to the output
          output.map((ast, childName) => (ast, name + childName))
      }

    /**
     * Top-Level expansion of a Term is slighly different the later levels. A the top it's always ident.map(id => ...)
     * if Ident is an option as opposed to OptionMap which it would be, in lower layers.
     *
     * Legend:
     *   T(x, [y,z])      := Term(x=name, children=List(y,z)), T-Opt=OptionalTerm I.e. term where optional=true
     *   P(a, b)          := Property(a, b) i.e. a.b
     *   M(a, v, P(v, b)) := Map(a, v, P(v, b)) or m.map(v => P(v, b))
     */
    def toAstTop(node: Term): List[(Ast, String)] = {
      node match {
        // Node without children
        // If leaf node, return the term, don't care about if it is optional or not
        case Term(name, _, Nil, _) =>
          val output = List(Ident(name, Quat.Generic))
          // For a single-element field, add field name to the output
          output.map(ast => (ast, name))

        // Product node not inside an option.
        // T( a, [T(b), T(c)] ) => [ a.b, a.c ]
        // (done?)         => [ P(a, b), P(a, c) ]
        // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
        // where T is Term and P is Property (in Ast) and [] is a list
        case Term(name, _, list, false) =>
          val output = list.flatMap(elem => toAst(elem, Ident(name, Quat.Generic)))
          // Do not add top level field to the output. Otherwise it would be x -> (x.width, xwidth), (x.height, xheight)
          output

        // Production node inside an Option
        // T-Opt( a, [T(b), T(c)] ) =>
        // [ a.map(v => v.b), a.map(v => v.c) ]
        // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
        // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]
        case Term(name, _, list, true) =>
          val idV = Ident("v", Quat.Generic)
          val output =
            for {
              elem <- list
              (newAst, name) <- toAst(elem, idV)
              // TODO Is this right? Should it be OptionTableMap?
            } yield (Map(Ident(name, Quat.Generic), idV, newAst), name)
          // Do not add top level field to the output. Otherwise it would be x -> (x.width, xwidth), (x.height, xheight)
          output
      }
    }

    private[getquill] def ofProduct[T: Type](side: ElaborationSide, baseName: String = "notused", udtBehavior: UdtBehavior = UdtBehavior.Leaf)(using Quotes) =
      base[T](Term(baseName, Branch), side, udtBehavior)

  end Term

  /** Go through all possibilities that the element might be and collect their fields */
  def collectFields[Fields, Types](node: Term, fieldsTup: Type[Fields], typesTup: Type[Types], side: ElaborationSide)(using Quotes): List[Term] = {
    import quotes.reflect.{Term => QTerm, _}

    (fieldsTup, typesTup) match {
      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].isProduct =>
        base[tpe](node, side) :: collectFields(node, Type.of[fields], Type.of[types], side)
      case (_, '[EmptyTuple]) => Nil
      case _                  => report.throwError("Cannot Derive Sum during Type Flattening of Expression:\n" + (fieldsTup, typesTup))
    }
  }

  @tailrec
  def flatten[Fields, Types](node: Term, fieldsTup: Type[Fields], typesTup: Type[Types], side: ElaborationSide, accum: List[Term] = List())(using Quotes): List[Term] = {
    import quotes.reflect.{Term => QTerm, _}

    def constValue[T: Type]: String =
      TypeRepr.of[T] match {
        case ConstantType(IntConstant(value))    => value.toString
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
              val childTerm = Term(Type.of[field].constValue, Branch, optional = true)
              // println(s"------ Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a product ----------")
              val baseTerm = base[tpe](childTerm, side)
              flatten(node, Type.of[fields], Type.of[types], side, baseTerm +: accum)
            else
              val childTerm = Term(Type.of[field].constValue, Leaf, optional = true)
              // println(s"------ Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a Leaf ----------")
              flatten(node, Type.of[fields], Type.of[types], side, childTerm +: accum)

      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].isProduct && Type.of[tpe].notOption =>
        val childTerm = Term(Type.of[field].constValue, Branch)
        // println(s"------ Non-Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a product ----------")
        val baseTerm = base[tpe](childTerm, side)
        flatten(node, Type.of[fields], Type.of[types], side, baseTerm +: accum)

      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].notOption =>
        val childTerm = Term(Type.of[field].constValue, Leaf)
        // println(s"------ Non-Optional field expansion ${Type.of[field].constValue.toString}:${TypeRepr.of[tpe].show} is a Leaf ----------")
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
   */
  def base[T: Type](term: Term, side: ElaborationSide, udtBehavior: UdtBehavior = UdtBehavior.Leaf)(using Quotes): Term = {
    import quotes.reflect.{Term => QTerm, _}

    // for errors/warnings
    def encDecText =
      side match
        case ElaborationSide.Encoding => "encodeable"
        case ElaborationSide.Decoding => "decodable"

    val isAutomaticLeaf =
      side match
        // Not sure why the UDT part is needed since it shuold always have a GenericEncoder/Decoder anyway
        case _ if (TypeRepr.of[T] <:< TypeRepr.of[io.getquill.Udt]) =>
          // println(s"------- TREATING UDT as Leaf ${Format.TypeOf[T]}")
          // If we are elaborating a UDT and are told to wrap normally, make sure that this is done
          // even if an encoder exists for the UDT. Otherwise, automatically treat the UDT as a Leaf entity
          // (since an encoder for it should have been derived by the macro that used UdtBehavior.Derive)
          udtBehavior match
            case UdtBehavior.Leaf   => true
            case UdtBehavior.Derive => false
        case ElaborationSide.Encoding =>
          // println(s"------- ALREADY EXISTS Encoder for ${Format.TypeOf[T]}")
          Expr.summon[GenericEncoder[T, _, _]].isDefined
        case ElaborationSide.Decoding =>
          // println(s"------- ALREADY EXISTS Decoder for ${Format.TypeOf[T]}")
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
            case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes } } =>
              val children = flatten(term, Type.of[elementLabels], Type.of[elementTypes], side)
              term.withChildren(children)
            // TODO Make sure you can summon a ColumnResolver if there is a SumMirror, otherwise this kind of decoding should be impossible
            case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes } } =>
              // Find field infos (i.e. Term objects) for all potential types that this coproduct could be
              val alternatives = collectFields(term, Type.of[elementLabels], Type.of[elementTypes], side)
              // Then merge them together to get one term representing all of their fields types.
              // Say you have a coproduct Shape -> (Square(width), Rectangle(width,height), Circle(radius))
              // You would get Term(width, height, radius)
              alternatives.reduce((termA, termB) => termA.merge[T](termB))
            case _ =>
              report.throwError(
                s"Although a mirror of the type ${Format.TypeOf[T]} can be summoned. It is not a sum-type, a product-type, or a ${encDecText} entity so it's fields cannot be understood in the structure-elaborator. It's mirror is ${Format.Expr(ev)}"
              )
        case None =>
          report.throwError(s"A mirror of the type ${Format.TypeOf[T]} cannot be summoned. It is not a sum-type, a product-type, or a ${encDecText} entity so it's fields cannot be understood in the structure-elaborator.")
  }

  private def productized[T: Type](side: ElaborationSide, baseName: String = "x")(using Quotes): Ast = {
    val lifted = base[T](Term(baseName, Branch), side).toAst
    val insert =
      if (lifted.length == 1)
        lifted.head._1
      else {
        CaseClass(lifted.map((ast, name) => (name, ast)))
      }
    insert
  }

  def ofProductType[T: Type](baseName: String, side: ElaborationSide)(using Quotes): List[Ast] = {
    val expanded = base[T](Term(baseName, Branch), side)
    expanded.toAst.map(_._1)
  }

  def ofArbitraryType[T: Type](baseName: String, side: ElaborationSide)(using Quotes): Ast =
    productized(side, baseName)

  /**
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
    val elaborated = ElaborateStructure.Term.ofProduct[T](side)
    // create a nested AST for the Term nest with the expected scalar tags inside
    val (_, nestedAst) = productValueToAst(elaborated)
    // create the list of (label, lift) for the expanded entities
    val lifts = liftsOfProductValue(elaborated, productValue)
    // return nested AST keyed by the lifts list
    TaggedLiftedCaseClass(nestedAst, lifts)
  }

  def decomposedProductValue[T: Type](side: ElaborationSide)(using Quotes) = {
    val elaborated = ElaborateStructure.Term.ofProduct[T](side)
    decomposedLiftsOfProductValue(elaborated)
  }

  def decomposedProductValueDetails[T: Type](side: ElaborationSide, udtBehavior: UdtBehavior)(using Quotes) =
    import quotes.reflect._

    def innerType(tpe: Type[_]) =
      tpe match
        case '[Option[t]] => Type.of[t]
        case _            => tpe

    def isOptional(tpe: Type[_]) =
      tpe match
        case '[Option[t]] => true
        case _            => false

    def summonElaboration[T: Type] =
      val elaboration = ElaborateStructure.Term.ofProduct[T](side, udtBehavior = udtBehavior)
      if (elaboration.typeType == Leaf)
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
    (components, elaboration.typeType)
  end decomposedProductValueDetails

  private[getquill] def liftsOfProductValue[T: Type](elaboration: Term, productValue: Expr[T])(using Quotes) =
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
            report.warning(s"The following the expression was typed `Any`: ${Format.Expr(showableExprPath)}. Will likely not be able to summon an encoder for this (the actual type was: ${Format.TypeOf[T]} in ${Format.TypeRepr(
              showableExprPath.asTerm.tpe
            )})  (the other param was ${Format.TypeOf[T]}.")
          '{ ${ exprPath(productValue) }.asInstanceOf[t] }
    }
    if (labels.length != pathLambdas.length)
      report.throwError(s"List of (${labels.length}) labels: ${labels} does not match list of (${paths.length}) paths that they represent: ${paths.map(Format.Expr(_))}")
    val outputs = labels.zip(paths)
    outputs.foreach { (label, exprPath) =>
      if (exprPath.asTerm.tpe =:= TypeRepr.of[Any])
        report.warning(s"`Any` value found for the path ${label} at the expression ${Format.Expr(exprPath)}. Will likely not be able to summon an encoder for this.")
    }
    outputs

  private[getquill] def decomposedLiftsOfProductValue[T: Type](elaboration: Term)(using Quotes): List[(Expr[T] => Expr[_], Type[_])] =
    DeconstructElaboratedEntityLevels[T](elaboration)

  /**
   * Flatten the elaboration from 'node' into a completely flat product type
   * Technicallly don't need Type T but it's very useful to know for errors and it's an internal API so I'll keep it for now
   */
  private[getquill] def productValueToAst[T: Type](node: Term /* i.e. the elaboration */ )(using Quotes): (String, Ast) =
    def toAstRec(node: Term, parentTerms: Chunk[String], topLevel: Boolean = false): (String, Ast) =
      def notTopLevel(termName: Chunk[String]) = if (topLevel) Chunk.empty else termName
      node match
        case Term(name, Leaf, _, _) =>
          // CC(foo: CC(bar: CC(baz: String))) should be: ScalarTag(foobarbaz, Source.UnparsedProperty("foo_bar_baz"))
          // the UnparsedProperty part is potentially used in batch queries for property naming
          val tagTerms = parentTerms :+ name
          val tagName = tagTerms.mkString
          // There could be variable names that have "$" in them e.g. anonymous tuples as in:
          //   foreach(List((foo,bar),(baz,blin))).map { case (a, b) => query[Update](...a...) }
          // so the batch identifier is unknown would manifest as x$1 etc... Make sure to at least remove $ from the variable name
          val tagFieldName = tagTerms.mkString("_")
          (name, ScalarTag(tagName, Source.UnparsedProperty(tagFieldName)))
        // On the top level, parent is "", and 1st parent in recursion is also ""
        case Term(name, Branch, list, false) =>
          val children = list.map(child => toAstRec(child, notTopLevel(parentTerms :+ name)))
          (name, CaseClass(children))
        // same logic for optionals
        case Term(name, Branch, list, true) =>
          val children = list.map(child => toAstRec(child, notTopLevel(parentTerms :+ name)))
          (name, OptionSome(CaseClass(children)))
        case _ =>
          quotes.reflect.report.throwError(s"Illegal generic schema: $node from type ${Type.of[T]}")
    toAstRec(node, Chunk.empty, true)

  extension [T](opt: Option[T])
    def getOrThrow(msg: String) = opt.getOrElse { throw new IllegalArgumentException(msg) }

  case class TaggedLiftedCaseClass[A <: Ast](caseClass: A, lifts: List[(String, Expr[_])]) {
    import java.util.UUID
    def uuid() = UUID.randomUUID.toString

    /** Replace keys of the tagged lifts with proper UUIDs */
    def reKeyWithUids(): TaggedLiftedCaseClass[A] = {
      def replaceKeys(newKeys: Map[String, String]): Ast =
        Transform(caseClass) {
          case ScalarTag(keyName, source) =>
            lazy val msg = s"Cannot find key: '${keyName}' in the list of replacements: ${newKeys}"
            ScalarTag(newKeys.get(keyName).getOrThrow(msg), source)
        }

      val oldAndNewKeys = lifts.map((key, expr) => (key, uuid(), expr))
      val keysToNewKeys = oldAndNewKeys.map((key, newKey, _) => (key, newKey)).toMap
      val newNewKeysToLifts = oldAndNewKeys.map((_, newKey, lift) => (newKey, lift))
      val newAst = replaceKeys(keysToNewKeys)
      TaggedLiftedCaseClass(newAst.asInstanceOf[A], newNewKeysToLifts)
    }
  }
}
