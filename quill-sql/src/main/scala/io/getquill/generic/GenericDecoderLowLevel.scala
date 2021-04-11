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

// trait GenericColumnResolver[ResultRow] {
//   def apply(resultRow: ResultRow, columnName: String): Int
// }

// trait GenericDecoder[ResultRow, T] extends ((Int, ResultRow) => T) {
//   def apply(i: Int, rr: ResultRow): T
// }

// trait GenericRowTyper[ResultRow, Co] {
//   def apply(rr: ResultRow): ClassTag[_]
// }

object GenericDecoderLowLevel {


  def flatten[Fields, Types, T, ResultRow](t: T, resultRow: ResultRow, fieldsTup: Type[Fields], typesTup: Type[Types])(using Quotes): List[Expr[_]] = {
    import quotes.reflect.{Term => QTerm, _}
    val ext = new TypeExtensions
    import ext._

    def constValue[T: Type]: String =
      TypeRepr.of[T] match {
        case ConstantType(IntConstant(value)) => value.toString
        case ConstantType(StringConstant(value)) => value.toString
        // Macro error
      }

    (fieldsTup, typesTup) match {
      // TODO These calls are expensive
      // do this first '[field *: fields], then do '[Option[tpe] *: types] internally

      case ('[field *: fields], '[Option[tpe] *: types]) if Type.of[tpe].isProduct =>
        ???

      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].isProduct && Type.of[tpe].notOption  =>
        ???

      case ('[field *: fields], '[Option[tpe] *: types]) =>
        ???

      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].notOption =>
        ???

      case (_, '[EmptyTuple]) => Nil
        ???

      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + (fieldsTup, typesTup))
    } 
  }

  def base[T: Type, ResultRow: Type](index: Int, resultRow: ResultRow)(using Quotes): T = {
    import quotes.reflect.{Term => QTerm, _}

    // if there is a decoder for the term, just return the term
    Expr.summon[Mirror.Of[T]] match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val childFields = flatten(index, resultRow, Type.of[elementLabels], Type.of[elementTypes])
            //TypeRepr.of[T].classSymbo
            ???


          // TODO Make sure you can summon a ColumnResolver if there is a SumMirror, otherwise this kind of decoding should be impossible
          case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            ???  

          case _ =>
            ???
            // Expr.summon[GenericDecoder[_, T]] match {
            //   case Some(decoder) => term
            //   case _ => report.throwError("Cannot Find Decoder or Expand a Product of the Type:\n" + ev.show)
            // }
        }
      }
      case None => 
        ???
        // Expr.summon[GenericDecoder[_, T]] match {
        //   case Some(decoder) => term
        //   case None => report.throwError(s"Cannot find derive or summon a decoder for ${Type.show[T]}")
        // }
    }
  }

}

