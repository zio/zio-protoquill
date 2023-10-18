package io.getquill.generic

import scala.quoted._

object WarnMac {
  inline def apply[F, T, Tail](msg: String): Unit = ${ applyImpl[F, T, Tail]('msg) }
  def applyImpl[F: Type, T: Type, Tail: Type](msg: Expr[String])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    import io.getquill.util.Format
    msg match {
      case Expr(str: String) =>
        println(s"${str} - ${Format.TypeRepr(TypeRepr.of[F])}: ${Format.TypeRepr(TypeRepr.of[T])} -> ${Format
            .TypeRepr(TypeRepr.of[Tail])}")
    }
    '{ () }
  }
}
