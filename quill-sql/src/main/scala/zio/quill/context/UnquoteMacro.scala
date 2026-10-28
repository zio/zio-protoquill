package zio.quill.context

import scala.quoted._
import zio.quill.Quoted
import zio.quill.Unquote

object UnquoteMacro {
  def apply[T: Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = {
    import quotes.reflect._
    '{
      Unquote[T](${ quoted }, ${ Expr(java.util.UUID.randomUUID().toString) }).unquote
    }
  }
}
