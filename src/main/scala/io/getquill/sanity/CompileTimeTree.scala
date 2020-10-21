package io.getquill.sanity

import scala.quoted._


object CompileTimeTree {
  inline def printUnderlyingArg(value: Any): String = ${printUnderlyingArgImpl('value)}
  def printUnderlyingArgImpl(value: Expr[Any])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_}
    Expr(value.unseal.underlyingArgument.seal.show)
  }
}