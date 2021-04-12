package io.getquill.generic

import io.getquill._
import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline}
import io.getquill.metaprog.TypeExtensions
import io.getquill.util.Format

trait GenericColumnResolver[ResultRow] {
  def apply(resultRow: ResultRow, columnName: String): Int
}

trait GenericDecoder[ResultRow, T] extends ((Int, ResultRow) => T) {
  def apply(i: Int, rr: ResultRow): T
}

trait GenericRowTyper[ResultRow, Co] {
  def apply(rr: ResultRow): ClassTag[_]
}

object GenericDecoder {

  def summonAndDecode[ResultRow: Type, T: Type](index: Expr[Int], resultRow: Expr[ResultRow])(using Quotes): Expr[T] = 
    import quotes.reflect.{Term => QTerm, _}
    Expr.summon[GenericDecoder[ResultRow, T]] match
      case Some(decoder) => '{ $decoder($index, $resultRow) }
      case _ => report.throwError(s"Cannot find decoder for the type: ${Format.TypeOf[T]}")

  def flatten[ResultRow: Type, Types: Type](index: Expr[Int], resultRow: Expr[ResultRow])(typesTup: Type[Types])(using Quotes): List[(Type[_], Expr[_])] = {
    import quotes.reflect.{Term => QTerm, _}
    val ext = new TypeExtensions
    import ext._

    typesTup match {
      case ('[tpe *: types]) if Type.of[tpe].isProduct =>
       val air = TypeRepr.of[tpe].classSymbol.get.caseFields.length
       (Type.of[tpe], summonAndDecode[ResultRow, tpe](index, resultRow)) :: flatten[ResultRow, types]('{$index + ${Expr(air)}}, resultRow)(Type.of[types])

      case ('[tpe *: types]) =>
        val air = TypeRepr.of[tpe].classSymbol.get.caseFields.length
        (Type.of[tpe], summonAndDecode[ResultRow, tpe](index, resultRow)) :: flatten[ResultRow, types]('{$index + 1}, resultRow)(Type.of[types])

      case ('[EmptyTuple]) => Nil

      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + typesTup)
    } 
  }

  def derived[T: Type, ResultRow: Type](using Quotes): Expr[GenericDecoder[ResultRow, T]] = {
    import quotes.reflect._
    // if there is a decoder for the term, just return the term
    '{
      new GenericDecoder[ResultRow, T] {
        override def apply(index: Int, resultRow: ResultRow): T = ${

          Expr.summon[Mirror.Of[T]] match
            case Some(ev) =>
              // Otherwise, recursively summon fields
              ev match {
                case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
                  val children = flatten('index, 'resultRow)(Type.of[elementTypes])
                  val types = children.map(_._1)
                  val terms = children.map(_._2)
                  val constructor = TypeRepr.of[T].typeSymbol.primaryConstructor

                  val construct =
                    if (TypeRepr.of[T] <:< TypeRepr.of[Tuple]) {
                      Apply(
                        TypeApply(
                          Select(New(TypeTree.of[T]), constructor),
                          types.map { tpe =>
                            tpe match
                              case '[tt] => TypeTree.of[tt]
                          }
                        ),
                        terms.map(_.asTerm)
                      )
                    } else {
                      Apply(
                        Select(New(TypeTree.of[T]), constructor),
                        terms.map(_.asTerm)
                      )
                    }
                  construct.asExprOf[T]
                  
                case _ => '{ ??? }
                  
              }
            
            case _ => '{ ??? }
              
        }
      }
    }
  }
}
