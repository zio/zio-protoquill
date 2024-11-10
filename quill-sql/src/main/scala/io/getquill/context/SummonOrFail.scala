package io.getquill.context

import io.getquill.util.Format

import scala.quoted.*

object SummonOrFail {
  def exprOf[T: Type](failMsg: String)(using Quotes): Expr[T] = {
    import quotes.reflect._
    val tpe = TypeRepr.of[T]
    Implicits.search(tpe) match {
      case res: ImplicitSearchSuccess => res.tree.asExprOf[T]
      case fail: ImplicitSearchFailure =>
        report.throwError(
          failMsg + "\n=============== Reason ===============\n" + fail.explanation
        )
    }
  }
}