package io.getquill.parser

import scala.quoted.{Type => TType, _}
import scala.quoted
import io.getquill.ast.{Ident => AIdent, Query => AQuery, _}
import io.getquill.metaprog.Extractors._
import scala.reflect.ClassTag;
import scala.reflect.classTag;
import io.getquill.quat.Quat
import io.getquill.util.Messages
import io.getquill.ReturnAction
import scala.util.Try
import io.getquill.util.Format
import io.getquill.util.StringUtil.section
import io.getquill.parser.DoSerialize
import io.getquill.ast
import io.getquill.util.CommonExtensions.Throwable._

object SerialHelper:
  import io.getquill.quat.{Quat => QQuat}
  object Ast:
    def fromSerialized(serial: String): Ast  = BooSerializer.Ast.deserialize(serial)
    def toSerialized(value: ast.Ast): String = BooSerializer.Ast.serialize(value)
    class Expr[T]:
      def apply(value: ast.Ast)(using Quotes, TType[T]) =
        import quotes.reflect._
        val serialized = SerialHelper.Ast.toSerialized(value)
        '{ SerialHelper.Ast.fromSerialized(${ quoted.Expr(serialized) }).asInstanceOf[T] }
      def unapply(expr: quoted.Expr[_])(using Quotes, TType[T]) =
        import quotes.reflect._
        expr match
          case '{ SerialHelper.Ast.fromSerialized(${ quoted.Expr(serialized) }).asInstanceOf[t] } =>
            Some(SerialHelper.Ast.fromSerialized(serialized).asInstanceOf[T])
          case _ =>
            None

  object Quat:
    def fromSerialized(serial: String): QQuat = BooSerializer.Quat.deserialize(serial)
    def toSerialized(quat: QQuat): String     = BooSerializer.Quat.serialize(quat)
    class Expr[T]:
      def apply(value: QQuat)(using Quotes, TType[T]) =
        import quotes.reflect._
        val serialized = SerialHelper.Quat.toSerialized(value)
        '{ SerialHelper.Quat.fromSerialized(${ quoted.Expr(serialized) }).asInstanceOf[T] }
      def unapply(expr: quoted.Expr[_])(using Quotes, TType[T]) =
        import quotes.reflect._
        expr match
          case '{ SerialHelper.Quat.fromSerialized(${ quoted.Expr(serialized) }).asInstanceOf[t] } =>
            Some(SerialHelper.Quat.fromSerialized(serialized).asInstanceOf[T])
          case _ =>
            None

  object QuatProduct:
    def fromSerialized(serial: String): QQuat.Product = BooSerializer.QuatProduct.deserialize(serial)
    def toSerialized(quat: QQuat.Product): String     = BooSerializer.QuatProduct.serialize(quat)
    class Expr[T]:
      def apply(value: QQuat.Product)(using Quotes, TType[T]) =
        import quotes.reflect._
        val serialized = SerialHelper.QuatProduct.toSerialized(value)
        '{ SerialHelper.QuatProduct.fromSerialized(${ quoted.Expr(serialized) }).asInstanceOf[T] }
      def unapply(expr: quoted.Expr[_])(using Quotes, TType[T]) =
        import quotes.reflect._
        expr match
          case '{ SerialHelper.QuatProduct.fromSerialized(${ quoted.Expr(serialized) }).asInstanceOf[t] } =>
            Some(SerialHelper.QuatProduct.fromSerialized(serialized).asInstanceOf[T])
          case _ =>
            None

  object BehaviorEnabled:
    def unapply(value: Expr[DoSerialize])(using Quotes): Boolean =
      import quotes.reflect._
      val memberSymbol = value.asTerm.tpe.termSymbol.memberType("BehaviorType")
      value.asTerm.select(memberSymbol).tpe <:< TypeRepr.of[io.getquill.parser.SerializationBehavior.SkipSerialize]

end SerialHelper
