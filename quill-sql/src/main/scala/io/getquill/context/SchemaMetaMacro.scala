package io.getquill.context

import scala.quoted._
import io.getquill.metaprog.Extractors._
import io.getquill.SchemaMeta
import io.getquill.Unquote
import io.getquill.util.LoadModule
import io.getquill.querySchema

object SchemaMetaMacro {

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
