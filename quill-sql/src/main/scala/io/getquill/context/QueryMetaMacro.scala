package io.getquill.context

import scala.quoted._
import io.getquill.parser.ParserFactory
import io.getquill._
import io.getquill.MetaDsl

object QueryMetaMacro {
  def embed[T: Type, R: Type](qm: Expr[MetaDsl], expand: Expr[Quoted[Query[T] => Query[R]]], extract: Expr[R => T])(using Quotes): Expr[QueryMeta[T, R]] = {
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    '{ QueryMeta[T, R]($expand, $uuid, $extract) }
  }
}
