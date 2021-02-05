package io.getquill.derived

import scala.quoted._
import io.getquill.ast.{ Ast, Map => AMap }
import io.getquill.derived.ElaborateQueryMeta.TaggedLiftedCaseClass

/** Testing Hook for ElaborateQueryMeta */
object ElaborateQueryMetaHook {

  /** An external hook to run the Elaboration with a given AST during runtime (mostly for testing). */
  inline def external[T](ast: Ast): AMap = ${ ElaborateQueryMeta.dynamic[T]('ast) }

  inline def ofCaseClassExternal[T](baseName: String, claseClassExpression: T): TaggedLiftedCaseClass = 
    ${ ElaborateQueryMetaHook.ofCaseClassExternalImpl('baseName, 'claseClassExpression) }

  def ofCaseClassExternalImpl[T: Type](baseName: Expr[String], claseClassExpression: Expr[T])(using Quotes): Expr[TaggedLiftedCaseClass] = {
    //val baseNameStr = 
    null
  }
}