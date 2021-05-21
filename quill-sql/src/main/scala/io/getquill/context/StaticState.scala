package io.getquill.context

import scala.quoted._
import io.getquill.Planter
import io.getquill.ReturnAction
import io.getquill.ast.Ast
import io.getquill.metaprog.PlanterExpr

case class StaticState(query: Unparticular.Query, rawLifts: List[PlanterExpr[?, ?]], returnAction: Option[ReturnAction])(queryAst: =>Ast):
  /** 
   * Plant all the lifts and return them.
   * NOTE: If this is used frequently would it be worth caching (i.e. since this object is immutable)
   * and splicing them might be expensive if it is done over and over again.
   */
  def lifts(using Quotes) = rawLifts.map(_.plant)
  def ast: Ast = queryAst