package io.getquill.derived

import scala.quoted._

object ElaborateMac {
  inline def elaborate[T <: Product](t: T): List[(String, Any)] = ${ elaborateImpl('t) }
  def elaborateImpl[T <: Product: Type](t: Expr[T])(using qctx: Quotes): Expr[List[(String, Any)]] = {
    val out = ElaborateQueryMeta.ofEntity[T](t)
    val outExpr = out.map((str, expr) => '{ (${Expr(str)}, $expr) })
    Expr.ofList(outExpr)
  }
}