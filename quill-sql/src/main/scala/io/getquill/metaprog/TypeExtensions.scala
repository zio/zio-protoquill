package io.getquill.metaprog

import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{Hidden, Visible}
import scala.deriving._
import scala.quoted._
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType

object TypeExtensions {

  extension (tpe: Type[_])(using Quotes)

    def constValue: String =
      import quotes.reflect._
      TypeRepr.of(using tpe) match {
        case ConstantType(IntConstant(value))    => value.toString
        case ConstantType(StringConstant(value)) => value.toString
        // Macro error
      }

    // As Quill-Row i.e. Product in the Quill sense is where you have a case-class for which there is no summonable decoder
    def isProduct: Boolean =
      import quotes.reflect._
      tpe match
        case '[tt] =>
          val hasEncoder = Expr.summon[GenericDecoder[_, _, tt, DecodingType.Specific]].isDefined
          TypeRepr.of(using tpe) <:< TypeRepr.of[Product] && !hasEncoder

    def notOption: Boolean =
      import quotes.reflect._
      !(TypeRepr.of(using tpe) <:< TypeRepr.of[Option[Any]])

  end extension

}
