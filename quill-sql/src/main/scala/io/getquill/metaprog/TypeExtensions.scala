package io.getquill.metaprog

import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{Hidden, Visible}
import scala.deriving._
import scala.quoted._

object TypeExtensions {

  extension (tpe: Type[_])(using Quotes) {

    def constValue: String = {
      import quotes.reflect._
      TypeRepr.of(using tpe) match {
        case ConstantType(IntConstant(value))    => value.toString
        case ConstantType(StringConstant(value)) => value.toString
        // Macro error
      }
    }

    def isProduct: Boolean = {
      import quotes.reflect._
      TypeRepr.of(using tpe) <:< TypeRepr.of[Product]
    }

    def notOption: Boolean = {
      import quotes.reflect._
      !(TypeRepr.of(using tpe) <:< TypeRepr.of[Option[Any]])
    }

    /** Cheap O(1) check for types that are guaranteed to have encoders/decoders in every context.
      * Uses TypeRepr.=:= instead of expensive Expr.summon implicit search. */
    def isKnownLeaf: Boolean = {
      import quotes.reflect._
      val repr = TypeRepr.of(using tpe)
      repr =:= TypeRepr.of[String] ||
      repr =:= TypeRepr.of[Int] ||
      repr =:= TypeRepr.of[Long] ||
      repr =:= TypeRepr.of[Short] ||
      repr =:= TypeRepr.of[Byte] ||
      repr =:= TypeRepr.of[Float] ||
      repr =:= TypeRepr.of[Double] ||
      repr =:= TypeRepr.of[Boolean] ||
      repr =:= TypeRepr.of[BigDecimal] ||
      repr =:= TypeRepr.of[Array[Byte]]
    }

  } // end extension

}
