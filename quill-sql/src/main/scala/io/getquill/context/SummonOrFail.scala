package io.getquill.context

import io.getquill.util.Format

import scala.quoted.*

object Summon {

  object OrLeft {
    def exprOf[T: Type](using Quotes): Either[String, Expr[T]] =
      exprOf[T](None)

    def exprOf[T: Type](sourceExpr: Expr[?])(using Quotes): Either[String, Expr[T]] =
      exprOf[T](Some(sourceExpr))

    private def exprOf[T: Type](sourceExpr: Option[Expr[?]])(using Quotes): Either[String, Expr[T]] = {
      import quotes.reflect._
      val tpe = TypeRepr.of[T]
      Implicits.search(tpe) match {
        case res: ImplicitSearchSuccess => Right(res.tree.asExprOf[T])
        case fail: ImplicitSearchFailure => Left(fail.explanation)
      }
    }
  }

  object OrFail {
    def exprOf[T: Type](failMsg: String)(using Quotes): Expr[T] =
      exprOf[T](failMsg, None)

    def exprOf[T: Type](failMsg: String, sourceExpr: Expr[?])(using Quotes): Expr[T] =
      exprOf[T](failMsg, Some(sourceExpr))

    private def exprOf[T: Type](failMsg: String, sourceExpr: Option[Expr[?]])(using Quotes): Expr[T] = {
      import quotes.reflect._
      val tpe = TypeRepr.of[T]
      Implicits.search(tpe) match {
        case res: ImplicitSearchSuccess => res.tree.asExprOf[T]
        case fail: ImplicitSearchFailure =>
          report.throwError(
            failMsg + s"\n=============== Cannot Summon ${Format.TypeOf[T]} because: ===============\n" + fail.explanation
          )
      }
    }
  }
}