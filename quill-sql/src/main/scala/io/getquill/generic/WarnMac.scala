package io.getquill.generic

import scala.quoted._


object WarnMac {
  inline def apply[F, T](msg: String): Unit = ${ applyImpl[F, T]('msg) }
  def applyImpl[F: Type, T: Type](msg: Expr[String])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    import io.getquill.util.Format
    msg match
      case Expr(str: String) => 
        println(s"${str} - ${Format.TypeRepr(TypeRepr.of[F])}: ${Format.TypeRepr(TypeRepr.of[T])}")
    '{ () }
  }
}