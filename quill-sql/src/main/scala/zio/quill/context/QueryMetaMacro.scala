package zio.quill.context

import scala.quoted._
import zio.quill.parser.ParserFactory
import zio.quill._

object QueryMetaMacro {
  def embed[T: Type, R: Type](expand: Expr[Quoted[Query[T] => Query[R]]], extract: Expr[R => T])(using Quotes): Expr[QueryMeta[T, R]] = {
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    '{ QueryMeta[T, R]($expand, $uuid, $extract) }
  }
}
