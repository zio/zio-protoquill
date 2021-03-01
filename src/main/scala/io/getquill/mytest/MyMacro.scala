package io.getquill.mytest

import scala.quoted._

object MyMacro {
  inline def getAndEncode[T](t: T): String = ${ getAndEncodeImpl('t) }
  def getAndEncodeImpl[T: Type](t: Expr[T])(using qctx: Quotes): Expr[String] = {
    import qctx.reflect._
    val enc = 
      Expr.summon[MyEncoder[T]] match
        case Some(enc) => enc
        case None => report.throwError("Can't find encoder")
    '{ $enc.encode($t) }
  }
}

