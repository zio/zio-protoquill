package miniquill.quoter

import scala.quoted._

object UnquoteMacro {
  def apply[T: Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = {
    import quotes.reflect._
    '{
      Unquote[T](${quoted}, ${Expr(java.util.UUID.randomUUID().toString)}).unquote
    }
  }
}
