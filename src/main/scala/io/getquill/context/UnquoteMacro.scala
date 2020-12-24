package io.getquill.context

import scala.quoted._
import io.getquill.quoter.Quoted
import io.getquill.quoter.Unquote

object UnquoteMacro {
  def apply[T: Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = {
    import quotes.reflect._
    '{
      Unquote[T](${quoted}, ${Expr(java.util.UUID.randomUUID().toString)}).unquote
    }
  }
}
