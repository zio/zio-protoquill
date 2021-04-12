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
import io.getquill.metaprog.TypeExtensions
import io.getquill.util.Format

class DecodeAlternate(using val quotes: Quotes) {
  import quotes.reflect.{Term => QTerm, _}

  def summonAndDecode[ResultRow: Type, T: Type](index: Expr[Int], resultRow: Expr[ResultRow]): Expr[T] = 
    Expr.summon[GenericDecoder[ResultRow, T]] match
      case Some(decoder) => '{ $decoder($index, $resultRow) }
      case _ => report.throwError(s"Cannot find decoder for the type: ${Format.TypeOf[T]}")

  def flatten[ResultRow: Type, Types: Type](index: Expr[Int], resultRow: Expr[ResultRow])(typesTup: Type[Types]): List[(TypeTree, Expr[_])] = {
    import quotes.reflect.{Term => QTerm, _}
    val ext = new TypeExtensions
    import ext._

    typesTup match {
      //case ('[tpe *: types]) if Type.of[tpe].isProduct =>
      //  val air = TypeRepr.of[tpe].classSymbol.get.caseFields.length
      //  (TypeTree.of[tpe], summonAndDecode[ResultRow, tpe](index, resultRow)) :: flatten[ResultRow, types]('{$index + ${Expr(air)}}, resultRow)(Type.of[types])

      case ('[tpe *: types]) =>
        val air = TypeRepr.of[tpe].classSymbol.get.caseFields.length
        (TypeTree.of[tpe], summonAndDecode[ResultRow, tpe](index, resultRow)) :: flatten[ResultRow, types]('{$index + 1}, resultRow)(Type.of[types])

      case ('[EmptyTuple]) => Nil

      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + typesTup)
    } 
  }

  def base[T: Type, ResultRow: Type](index: Expr[Int], resultRow: Expr[ResultRow]): Expr[T] = {
    // if there is a decoder for the term, just return the term
    Expr.summon[Mirror.Of[T]] match
      case Some(ev) =>
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val children = flatten(index, resultRow)(Type.of[elementTypes])
            val types = children.map(_._1)
            val terms = children.map(_._2)
            val constructor = TypeRepr.of[T].typeSymbol.primaryConstructor
            val construct =
              Apply(
                TypeApply(
                  Select(New(TypeTree.of[T]), constructor),
                  types
                ),
                terms.map(_.asTerm)
              )
            construct.asExprOf[T]
            
          case _ => report.throwError("Tuple decoder could not be summoned")
        }
      
      case _ => 
        report.throwError("Tuple decoder could not be summoned")
  }
}

object DecodeAlternate {
  inline def apply[T, ResultRow](index: Int, resultRow: ResultRow): T = ${ applyImpl[T, ResultRow]('index, 'resultRow) }
  def applyImpl[T: Type, ResultRow: Type](index: Expr[Int], resultRow: Expr[ResultRow])(using quotes: Quotes): Expr[T] =
    import quotes.reflect._
    new DecodeAlternate().base[T, ResultRow](index, resultRow)
}