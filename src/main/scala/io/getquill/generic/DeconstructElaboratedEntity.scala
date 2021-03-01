package io.getquill.generic

import io.getquill._
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
import io.getquill.parser.TastyMatchers

object DeconstructElaboratedEntity {
  def apply(elaboration: Term, entity: Expr[_])(using qctx: Quotes) =
    new DeconstructElaboratedEntity(using qctx).apply(elaboration, entity)
}

// TODO Explain this is a specific elaborator used for Case Class Lifts
private[getquill] class DeconstructElaboratedEntity(using val qctx: Quotes) extends TastyMatchers {
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

  def apply(elaboration: Term, entity: Expr[_]): List[(Expr[_], String)] = {
    val elaborations = elaborateObjectRecurse(elaboration, entity, true)
    elaborations.map((expr, name) => (flattenOptions(expr), name))
  }

  // Note: Not sure if always appending name + childName is right to do. When looking
  // up fields by name with sub-sub Embedded things going to need to look into that
  private[getquill] def elaborateObjectRecurse(node: Term, expr: Expr[_], topLevel: Boolean = false): List[(Expr[_], String)] = {
    def emptyIfTop(str: String) = if(topLevel) "" else str

    (expr, node) match {
      // If leaf node, return the term, don't care about if it is optional or not
      case (_, Term(name, _, Nil, _)) =>
        List((expr, name))

      // Product node not inside an option
      // T( a, [T(b), T(c)] ) => [ a.b, a.c ] 
      // (done?)         => [ P(a, b), P(a, c) ] 
      // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
      // where T is Term and P is Property (in Ast) and [] is a list
      case (field, Term(name, _, childProps, false)) =>
        // TODO For coproducts need to check that the childName method actually exists on the type and
        // exclude it if it does not
        val output =
          childProps.flatMap { 
            childTerm =>
              val expr = field `.` (childTerm.name)
              elaborateObjectRecurse(childTerm, expr)
          }
        output.map((expr, childName) => (expr, emptyIfTop(name) + childName))

      // Production node inside an Option
      // T-Opt( a, [T(b), T(c)] ) => 
      // [ a.map(v => v.b), a.map(v => v.c) ] 
      // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
      // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]

      case ('{ ($optField: Option[t]) }, Term(name, _, childProps, true)) =>
        // def innerType[IT: Type]: Type[_] =
        //   Type.of[IT] match
        //     case '[Option[t]] => Type.of[t]
        //     case _ => tpe

        // val innerType = innerType[outerT]

        
        val output =
          // TODO For coproducts need to check that the childName method actually exists on the type and
          // exclude it if it does not
          childProps.flatMap { 
            childTerm => 
              // In order to be able to flatten optionals in the flattenOptionals later, we need ot make
              // sure that the method-type in the .map function below is 100% correct. That means we
              // need to lookup what the type of the field of this particular member should actually be.
              val tpe = TypeRepr.of[t]
              println(s"Get member '${childTerm.name}' of ${Printer.TypeReprShortCode.show(tpe)}")
              val memField = tpe.classSymbol.get.memberField(childTerm.name)
              val memeType = tpe.memberType(memField)
              println(s"MemField of ${childTerm.name} is ${memField}: ${Printer.TypeReprShortCode.show(memeType)}")
              memeType.asType match
                // If the nested field is itself optional, need to account for immediate flattening
                case '[Option[mt]] =>
                  val expr = '{ $optField.flatMap[mt](prop => ${('prop `.` (childTerm.name)).asExprOf[Option[mt]]}) }
                  elaborateObjectRecurse(childTerm, expr)
                case '[mt] =>
                  val expr = '{ $optField.map[mt](prop => ${('prop `.` (childTerm.name)).asExprOf[mt]}) }
                  elaborateObjectRecurse(childTerm, expr)
          }
        output.map((expr, childName) => (expr, emptyIfTop(name) + childName))

      case _ =>
          report.throwError(s"Illegal state during reducing expression term: '${node}' and expression: '${expr.show}'")
    }
  }

}