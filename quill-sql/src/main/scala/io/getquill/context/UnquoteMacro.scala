package io.getquill.context

import scala.quoted._
import io.getquill.Quoted
import io.getquill.Unquote

object UnquoteMacro {
  def apply[T: Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = {
    import quotes.reflect._
    '{
      Unquote[T](${ quoted }, ${ Expr(java.util.UUID.randomUUID().toString) }).unquote
    }
  }
}
