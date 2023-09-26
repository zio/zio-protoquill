package io.getquill.generic

import io.getquill.ast.*
import io.getquill.context.Execution.ElaborationBehavior

import scala.quoted.*
import io.getquill.context.sql.norm.SimplifyFilterTrue

object ElaborateTrivial {
  def apply(eb: ElaborationBehavior)(ast: Ast) =
    eb match {
      // if the AST is a Query, e.g. Query(Entity[Person], ...) we expand it out until something like
      // Map(Query(Entity[Person], ...), x, CaseClass(name: x.name, age: x.age)). This was based on the Scala2-Quill
      // flatten method in ValueProjection.scala. Technically this can be performed in the SqlQuery from the Quat info
      // but the old mechanism is still used because the Quat information might not be there.
      case ElaborationBehavior.Elaborate =>
        val x = Ident("x", ast.quat)
        Map(ast, x, x)
      case ElaborationBehavior.Skip =>
        ast
    }
}
