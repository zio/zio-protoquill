package io.getquill.metaprog

import io.getquill._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._

class TypeExtensions(using qctx: Quotes) { self =>
  import qctx.reflect._
  
  implicit class TypeExt(tpe: Type[_]) {
    def constValue = self.constValue(tpe)
    def isProduct = self.isProduct(tpe)
  }

  def constValue(tpe: Type[_]): String =
    TypeRepr.of(using tpe) match {
      case ConstantType(IntConstant(value)) => value.toString
      case ConstantType(StringConstant(value)) => value.toString
      // Macro error
    }
  def isProduct(tpe: Type[_]): Boolean =
    TypeRepr.of(using tpe) <:< TypeRepr.of[Product]
}