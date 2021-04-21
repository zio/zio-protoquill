package io.getquill.metaprog


import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._

object TypeExtensions {

  extension (tpe: Type[_])(using Quotes)

    def constValue: String =
      import quotes.reflect._
      TypeRepr.of(using tpe) match {
        case ConstantType(IntConstant(value)) => value.toString
        case ConstantType(StringConstant(value)) => value.toString
        // Macro error
      }

    def isProduct: Boolean =
      import quotes.reflect._
      TypeRepr.of(using tpe) <:< TypeRepr.of[Product]

    def notOption: Boolean =
      import quotes.reflect._
      !(TypeRepr.of(using tpe) <:< TypeRepr.of[Option[Any]])

  end extension

}
