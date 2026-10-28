package zio.quill.context

import scala.quoted._
import zio.quill.metaprog.Extractors._

import zio.quill.SchemaMeta
import zio.quill.Unquote
import zio.quill.util.Load
import zio.quill.querySchema

object SchemaMetaMacro {

  // inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] =
  // SchemaMeta(quote { querySchema[T](entity, columns: _*) }, "1234") // TODO Don't need to generate a UID here.It can be static.
  def apply[T](entity: Expr[String], columns: Expr[Seq[(T => (Any, String))]])(using Quotes, Type[T]): Expr[SchemaMeta[T]] = {
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    val exprs =
      (columns match {
        case GenericSeq(argsExprs) => argsExprs
      }).toList
    val quote = QuoteMacro('{ querySchema[T]($entity, $columns: _*) })
    '{ SchemaMeta($quote, $uuid) }
  }
}
