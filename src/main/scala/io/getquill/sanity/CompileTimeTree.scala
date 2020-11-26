package io.getquill.sanity

import scala.quoted._


object CompileTimeTree {
  inline def printUnderlyingArg(value: Any): String = ${printUnderlyingArgImpl('value)}
  def printUnderlyingArgImpl(value: Expr[Any])(using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(Term.of(value).underlyingArgument.asExpr.show)
  }
}