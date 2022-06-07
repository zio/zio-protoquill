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
import io.getquill.generic.ElaborateStructure.Term
import io.getquill.metaprog.Extractors
import io.getquill.util.Format
import io.getquill.util.Interpolator2
import io.getquill.util.Messages.TraceType

object DeconstructElaboratedEntityLevels {
  def apply[ProductCls: Type](elaboration: Term)(using Quotes) =
    withTerms[ProductCls](elaboration).map((term, func, tpe) => (func, tpe))

  def withTerms[ProductCls: Type](elaboration: Term)(using Quotes) =
    new DeconstructElaboratedEntityLevels().apply[ProductCls](elaboration)

}

// TODO Unify this with DeconstructElaboratedEntities. This will generate the fields
// and the labels can be generated separately and zipped in the case oc DeconstructElaboratedEntity
private[getquill] class DeconstructElaboratedEntityLevels(using val qctx: Quotes):
  import qctx.reflect._
  import io.getquill.metaprog.Extractors._
  import io.getquill.generic.ElaborateStructure.Term

  private def isOption[T: Type] =
    import quotes.reflect._
    TypeRepr.of[T] <:< TypeRepr.of[Option[Any]]

  System.setProperty("quill.trace.enabled", "true")
  System.setProperty("quill.trace.types", "elab")
  io.getquill.util.Messages.resetCache()

  val interp = new Interpolator2(TraceType.Elaboration, 1)
  import interp._

  sealed trait ElaboratedField
  object ElaboratedField:
    private def create(tpe: TypeRepr, fieldName: String) =
      val typeSymbol = tpe.typeSymbol
      typeSymbol.methodMembers.find(m => m.name == fieldName && m.paramSymss == List()).map(ZeroArgsMethod(_))
        .orElse(typeSymbol.fieldMembers.find(m => m.name == fieldName).map(Field(_)))
        .getOrElse(NotFound)

    case class ZeroArgsMethod(symbol: Symbol) extends ElaboratedField
    case class Field(symbol: Symbol) extends ElaboratedField
    case object NotFound extends ElaboratedField

    def resolve(tpe: TypeRepr, fieldName: String, term: Term) =
      ElaboratedField.create(tpe, fieldName) match
        case ZeroArgsMethod(sym) => (sym, tpe.widen.memberType(sym).widen)
        case Field(sym)          => (sym, tpe.widen.memberType(sym).widen)
        case NotFound            => report.throwError(s"Cannot find the field (or zero-args method) $fieldName in the ${tpe.show} term: $term")

  end ElaboratedField

  def apply[ProductCls: Type](elaboration: Term): List[(Term, Expr[ProductCls] => Expr[_], Type[_])] =
    // Don't know if a case where the top-level elaborate thing can be an optional but still want to add the check
    val topLevelOptional = isOption[ProductCls]
    recurseNest[ProductCls](elaboration, topLevelOptional).asInstanceOf[List[(Term, Expr[ProductCls] => Expr[_], Type[_])]]

  // TODO Do we need to include flattenOptions?
  // Given Person(name: String, age: Int)
  // Type TypeRepr[Person] and then List(Expr[Person => Person.name], Expr[Person => Person.age])
  def recurseNest[Cls: Type](node: Term, optionalAncestor: Boolean): List[(Term, Expr[_] => Expr[_], Type[_])] =
    trace"-------> Elaborating ${node.name}:${Format.TypeOf[Cls]} <-------" andContinue {
      // For example (given Person(name: String, age: Int)):
      // (Term(Person.name), Person => Person.name, String)
      // Or for nested entities (given Person(name: Name, age: Int))
      // (Term(Person.name), Person => Name, Name)

      val elaborations = elaborateObjectOneLevel[Cls](node)

      trace"Computed Elaborations: ${elaborations.map(_._3).map(io.getquill.util.Format.TypeRepr(_))}".andLog()
      elaborations.flatMap { (fieldTerm, fieldGetter, fieldTypeRepr) =>
        val fieldType = fieldTypeRepr.widen.asType
        val nextIsOptional = optionalAncestor || isOption[Cls]
        fieldType match
          case '[tt] =>
            val childFields = recurseNest[tt](fieldTerm, nextIsOptional)
            childFields match
              // On a child field e.g. Person.age return the getter that we previously found for it since
              // it will not have any children on the nextlevel
              case Nil =>
                List((fieldTerm, fieldGetter, fieldType)).asInstanceOf[List[(Term, Expr[Any] => Expr[_], Type[_])]]

              // If there are fields on the next level e.g. Person.Name then go from:
              // Person => Name to Person => Name.first, Person => Name.last by swapping in Person
              // i.e. p: Person => (Person => Name)(p).first, p: Person => (Person => Name)(p).last
              // This will recursive over these products a second time and then merge the field gets
              case _ =>
                // Note that getting the types to line up here is very tricky.
                val output =
                  recurseNest[tt](fieldTerm, nextIsOptional).map { (childTerm, nextField, nextType) =>
                    trace"Computing child term ${Format.Type(fieldType)}.${childTerm.name}:${Format.Type(nextType)}".andLog()

                    val castFieldGetter = fieldGetter.asInstanceOf[Expr[Any] => Expr[_]] // e.g. Person => Person.name (where name is a case class Name(first: String, last: String))
                    val castNextField = nextField.asInstanceOf[Expr[Any] => Expr[_]] // e.g. Name => Name.first
                    // e.g. nest Person => Person.name into Name => Name.first to get Person => Person.name.first
                    val pathToField =
                      // When Cls := Person(name: Name) and Name(first: String, last: String) ...
                      (outerClass: Expr[Cls]) =>
                        castNextField(castFieldGetter(outerClass))

                    lazy val debugInput = '{ (outerClass: Cls) => ${ pathToField('outerClass) } }
                    trace"Path to field '${nextField}' is: ${Format.Expr(debugInput)}".andLog()
                    // if you have Person(name: Option[Name]), Name(first: String, last: String) then the fieldTypeRepr is going to be Option[Name]
                    // that means that the child type (which is going to be person.name.map(_.first)) needs to be Option[String] instead of [String]
                    val childType =
                      (fieldType, nextType) match
                        case ('[Option[ft]], '[Option[nt]]) => nextType
                        case ('[Option[ft]], '[nt]) =>
                          val output = optionalize(nextType)
                          println(s"Optionalizing nextType ${Format.Type(nextType)} into ${Format.Type(output)}")
                          output
                        case ('[ft], '[Option[nt]]) =>
                          lazy val pathToFieldShowable =
                            '{ (classShowable: Cls) => ${ pathToField('classShowable) } }
                          lazy val fieldGetterShowable =
                            '{ (classGettable: Cls) => ${ fieldGetter('classGettable) } }
                          report.throwError(
                            s"Child Type ${Format.TypeOf[nt]} of the expression ${Format.Expr(pathToFieldShowable)} " +
                              s"is not optional but the parent type ${Format.TypeOf[ft]} of the parent expression ${Format.Expr(fieldGetterShowable)}" +
                              s"is. This should not be possible.\n" +
                              s"== The Parent is: ${(node)}\n" +
                              s"== The Parent field is: ${(fieldTerm)}\n" +
                              s"== The Child field is: ${(childTerm)}\n" +
                              s"== The field is: ${Format.Expr(fieldGetterShowable)}\n" +
                              s"== The objects are: ${Format.TypeOf[ft]} -> ${Format.Type(nextType)}"
                          )
                        case _ => nextType
                    (childTerm, pathToField, childType)
                  }
                // println(s"====== Nested Getters: ${output.map(_.show)}")
                output.asInstanceOf[List[(Term, Expr[Any] => Expr[_], Type[_])]]
      }
    }
  end recurseNest

  private[getquill] def optionalize(tpe: Type[_]) =
    tpe match
      case '[t] => Type.of[Option[t]]

  private[getquill] def flattenOptions(expr: Expr[_])(using Quotes): Expr[_] =
    import quotes.reflect._
    expr.asTerm.tpe.asType match
      case '[Option[Option[t]]] =>
        // println(s"~~~~~~~~~~~~~ Option For ${Printer.TreeShortCode.show(expr.asTerm)} ~~~~~~~~~~~~~")
        flattenOptions('{ ${ expr.asExprOf[Option[Option[t]]] }.flatten[t] })
      case _ =>
        // println(s"~~~~~~~~~~~~~ Non-Option For ${Printer.TreeShortCode.show(expr.asTerm)} ~~~~~~~~~~~~~")
        expr

  private[getquill] def elaborateObjectOneLevel[Cls: Type](node: Term): List[(Term, Expr[Cls] => Expr[_], TypeRepr)] = {
    val clsType = TypeRepr.of[Cls]
    val typeIsOptional = TypeRepr.of[Cls] <:< TypeRepr.of[Option[Any]]
    trace"Elaborating one level. ${node.name} of ${Format.TypeOf[Cls]}"
    node match
      // If leaf node, don't need to do anything since high levels have already returned this field
      case term @ Term(name, _, Nil, _) =>
        trace"(Leaf Non-Option) No props found for ${Format.TypeOf[Cls]}".andLog()
        List()

      // Product node not inside an option
      // T( a, [T(b), T(c)] ) => [ a.b, a.c ]
      // (done?)         => [ P(a, b), P(a, c) ]
      // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
      // where T is Term and P is Property (in Ast) and [] is a list
      case (Term(name, _, childProps, _)) if !typeIsOptional =>
        // TODO For coproducts need to check that the childName method actually exists on the type and
        // exclude it if it does not
        trace"(Node Non-Option) Mapping props of: ${Format.TypeOf[Cls]} is ${childProps.map(_.name)}".andLog()
        childProps.map { childTerm =>
          val (memberSymbol, memberType) = ElaboratedField.resolve(clsType, childTerm.name, childTerm) // for Person, Person.name.type
          trace"(Node Non-Option) MemField of: ${childTerm.name} is `${memberSymbol}:${Printer.TypeReprShortCode.show(memberType)}`".andLog()
          memberType.asType match
            case '[t] =>
              val expr = (field: Expr[Cls]) => (field `.(caseField)` (childTerm.name)).asExprOf[t]
              (
                childTerm,
                expr, // for Person, Person.name
                memberType
              )
        }

      // Production node inside an Option
      // T-Opt( a, [T(b), T(c)] ) =>
      // [ a.map(v => v.b), a.map(v => v.c) ]
      // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
      // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]
      case Term(name, _, childProps, _) if typeIsOptional =>
        // TODO For coproducts need to check that the childName method actually exists on the type and
        // exclude it if it does not
        trace"(Node Option) Mapping props of: ${Format.TypeOf[Cls]} is ${childProps.map(_.name)}".andLog()
        childProps.map {
          childTerm =>

            // In order to be able to flatten optionals in the flattenOptionals later, we need to make
            // sure that the method-type in the .map function below is 100% correct. That means we
            // need to lookup what the type of the field of this particular member should actually be.
            val rootType = `Option[...[t]...]`.innerT(Type.of[Cls])
            val rootTypeRepr =
              rootType match
                case '[t] => TypeRepr.of[t]
            val (memField, memeType) = ElaboratedField.resolve(rootTypeRepr, childTerm.name, childTerm)
            trace"(Node Option) MemField of: ${childTerm.name} is ${memField}: ${Printer.TypeReprShortCode.show(memeType)}".andLog()
            (Type.of[Cls], rootType) match
              case ('[cls], '[root]) =>
                memeType.asType match
                  // If the nested field is itself optional, need to account for immediate flattening
                  case '[Option[mt]] =>
                    val expr = (optField: Expr[cls]) => '{ ${ flattenOptions(optField).asExprOf[Option[root]] }.flatMap[mt](optionProp => ${ ('optionProp `.(caseField)` (childTerm.name)).asExprOf[Option[mt]] }) }
                    lazy val traceInput = '{ (optField: cls) => ${ expr('optField) } }
                    trace"(Node Option) '[Option[mt]] Mapping: asExprOf: ${childTerm.name} into ${Format.TypeOf[Option[mt]]} in ${Format.Expr(traceInput)}".andLog()
                    (
                      childTerm,
                      expr.asInstanceOf[Expr[Cls] => Expr[_]],
                      memeType
                    )
                  case '[mt] =>
                    val expr = (optField: Expr[cls]) => '{ ${ flattenOptions(optField).asExprOf[Option[root]] }.map[mt](regProp => ${ ('regProp `.(caseField)` (childTerm.name)).asExprOf[mt] }) }
                    lazy val traceInput = '{ (optField: cls) => ${ expr('optField) } }
                    trace"(Node Option) ['mt] Mapping: asExprOf: ${childTerm.name} into ${Format.TypeOf[mt]} in ${Format.Expr(traceInput)}".andLog()
                    (
                      childTerm,
                      expr.asInstanceOf[Expr[Cls] => Expr[_]],
                      memeType
                    )
        }

      case _ =>
        report.throwError(s"Illegal state during reducing expression term: '${node}' and type: '${io.getquill.util.Format.TypeRepr(clsType)}'")
    end match
  }

end DeconstructElaboratedEntityLevels
