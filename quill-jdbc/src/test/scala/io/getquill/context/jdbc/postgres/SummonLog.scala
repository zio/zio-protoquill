package io.getquill.context.jdbc.postgres

import scala.quoted._
import io.getquill.util.Format

object SummonLog {
  inline def apply[T]: Unit = ${ applyImpl[T] }
  def applyImpl[T: scala.quoted.Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val expr = Expr.summon[T]
    val tpeStr = expr.map(e => Format.TypeRepr(e.asTerm.tpe)).getOrElse("No type found")
    report.warning(s"Summoning: $tpeStr")
    '{ () }
  }
}
