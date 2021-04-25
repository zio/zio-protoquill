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
import io.getquill.generic.ElaborateStructure.Term
import io.getquill.metaprog.Extractors

object DeconstructElaboratedEntityLevels {
  def apply[ProductCls: Type](elaboration: Term)(using qctx: Quotes) =
    new DeconstructElaboratedEntityLevels(using qctx).apply[ProductCls](elaboration)
}

// TODO Explain this is a specific elaborator used for Case Class Lifts
private[getquill] class DeconstructElaboratedEntityLevels(using val qctx: Quotes) extends Extractors {
  import qctx.reflect._
  import io.getquill.generic.ElaborateStructure.Term

  private[getquill] def flattenOptions(expr: Expr[_]): Expr[_] = {
    expr.asTerm.tpe.asType match {
      case '[Option[Option[t]]] => 
        //println(s"~~~~~~~~~~~~~ Option For ${Printer.TreeShortCode.show(expr.asTerm)} ~~~~~~~~~~~~~")
        flattenOptions('{ ${expr.asExprOf[Option[Option[t]]]}.flatten[t] })
      case _ =>
        //println(s"~~~~~~~~~~~~~ Non-Option For ${Printer.TreeShortCode.show(expr.asTerm)} ~~~~~~~~~~~~~")
        expr
    }    
  }

  def apply[ProductCls: Type](elaboration: Term): List[Expr[ProductCls => _]] = {
    recurseNest[ProductCls](elaboration).asInstanceOf[List[Expr[ProductCls => _]]]
  }

  // TODO Need to include flattenOptions
  // Given Person(name: String, age: Int)
  // Type TypeRepr[Person] and then List(Expr[Person => Person.name], Expr[Person => Person.age])
  def recurseNest[Cls: Type](node: Term): List[Expr[Any => Any]] = {
    // For example (given Person(name: String, age: Int)):
    // (Term(Person.name), Person => Person.name, String)
    // Or for nested entities (given Person(name: Name, age: Int))
    // (Term(Person.name), Person => Name, Name)
    println(s"---------------> Entering: ${node} <----------------")
    val elaborations = elaborateObjectOneLevel[Cls](node)
    println(s"Elaborations: ${elaborations.map(_._3).map(io.getquill.util.Format.TypeRepr(_))}")
    elaborations.flatMap { (fieldTerm, fieldGetter, returnTpe) =>
      returnTpe.asType match
        case '[tt] =>
          val childFields = recurseNest[tt](fieldTerm)
          childFields match
            // On a child field e.g. Person.age return the getter that we previously found for it since 
            // it will not have any children on the nextlevel
            case Nil => 
              println(s"====== Leaf Getter: ${fieldGetter.show}")
              List(fieldGetter).asInstanceOf[List[Expr[Any => Any]]]

            // If there are fields on the next level e.g. Person.Name then go from:
            // Person => Name to Person => Name.first, Person => Name.last by swapping in Person
            // i.e. p: Person => (Person => Name)(p).first, p: Person => (Person => Name)(p).last
            // This will recursive over these products a second time and then merge the field gets
            case _ =>
              // Note that getting the types to line up here is very trickey. Leaving printline
              // statements in for now
              val output = 
                recurseNest[tt](fieldTerm).map { nextField =>
                  val castFieldGetter = fieldGetter.asInstanceOf[Expr[Any => Any]]
                  val castNextField = nextField.asInstanceOf[Expr[Any => Any]]
                  val sub = '{ $castFieldGetter.andThen[Any](ttValue => ${
                    //println(s"ttValue is: ${'ttValue.asTerm.underlyingArgument.show}")
                    castNextField
                  }(ttValue.asInstanceOf[tt])) }
                  //println(s"~~~~~~~~~~~ Recurse Expr: ${io.getquill.util.Format.Expr(sub)}")
                  sub
                }
              //println(s"====== Nested Getters: ${output.map(_.show)}")
              output.asInstanceOf[List[Expr[Any => Any]]]
    }
  }

  // Note: Not sure if always appending name + childName is right to do. When looking
  // up fields by name with sub-sub Embedded things going to need to look into that
  private[getquill] def elaborateObjectOneLevel[Cls: Type](node: Term): List[(Term, Expr[Cls => _], TypeRepr)] = {
    val clsType = TypeRepr.of[Cls]
    node match {
      // If leaf node, don't need to do anything since high levels have already returned this field
      case term @ Term(name, _, Nil, _) =>
        println(s"For Term: ${name} - Elaborate Object into Empty List")
        List()

      // Product node not inside an option
      // T( a, [T(b), T(c)] ) => [ a.b, a.c ] 
      // (done?)         => [ P(a, b), P(a, c) ] 
      // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
      // where T is Term and P is Property (in Ast) and [] is a list
      case (Term(name, _, childProps, false)) =>
        // TODO For coproducts need to check that the childName method actually exists on the type and
        // exclude it if it does not
        val output = 
          childProps.map { childTerm =>
              (
                childTerm, 
                '{ (field: Cls) => ${ ('field `.` (childTerm.name)) }  }, // for Person, Person.name
                {
                  // or classSymbol.get?
                  val memberSymbol = clsType.widen.typeSymbol.memberField(childTerm.name)  // for Person, Person.name.type
                  clsType.memberType(memberSymbol)
                }
              )
          }
        println(s"For Term: ${name} - Elaborate Object Into: ${output.map(_._3).map(io.getquill.util.Format.TypeRepr(_))}")
        output

      // Production node inside an Option
      // T-Opt( a, [T(b), T(c)] ) => 
      // [ a.map(v => v.b), a.map(v => v.c) ] 
      // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
      // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]

      case Term(name, _, childProps, true) if TypeRepr.of[Cls] <:< TypeRepr.of[Option[Any]] =>
        // def innerType[IT: Type]: Type[_] =
        //   Type.of[IT] match
        //     case '[Option[t]] => Type.of[t]
        //     case _ => tpe

        // val innerType = innerType[outerT]

        
        // TODO For coproducts need to check that the childName method actually exists on the type and
        // exclude it if it does not
        childProps.map { 
          childTerm => 
            // In order to be able to flatten optionals in the flattenOptionals later, we need ot make
            // sure that the method-type in the .map function below is 100% correct. That means we
            // need to lookup what the type of the field of this particular member should actually be.
            val tpe = 
              Type.of[Cls] match
                case '[Option[t]] => TypeRepr.of[t]
            //println(s"Get member '${childTerm.name}' of ${Printer.TypeReprShortCode.show(tpe)}")
            val memField = tpe.classSymbol.get.memberField(childTerm.name)
            val memeType = tpe.memberType(memField)
            //println(s"MemField of ${childTerm.name} is ${memField}: ${Printer.TypeReprShortCode.show(memeType)}")

            Type.of[Cls] match
              case '[Option[t]] =>
                memeType.asType match
                  // If the nested field is itself optional, need to account for immediate flattening
                  case '[Option[mt]] =>
                    val expr = '{ (optField: Option[t]) => optField.flatMap[mt](prop => ${('prop `.` (childTerm.name)).asExprOf[Option[mt]]}) }
                    (
                      childTerm, 
                      expr.asInstanceOf[Expr[Cls => _]],
                      memeType
                    )
                  case '[mt] =>
                    val expr = '{ (optField: Option[t]) => optField.map[mt](prop => ${('prop `.` (childTerm.name)).asExprOf[mt]}) }
                    (
                      childTerm, 
                      expr.asInstanceOf[Expr[Cls => _]],
                      memeType
                    )
        }

      case _ =>
          report.throwError(s"Illegal state during reducing expression term: '${node}' and type: '${io.getquill.util.Format.TypeRepr(clsType)}'")
    }
  }

}