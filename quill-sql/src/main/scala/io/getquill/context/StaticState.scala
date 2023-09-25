package io.getquill.context

import scala.quoted._
import io.getquill.Planter
import io.getquill.ReturnAction
import io.getquill.ast.Ast
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.Idiom

case class StaticState(
  query: Unparticular.Query,
  rawLifts: List[PlanterExpr[?, ?, ?]],
  returnAction: Option[ReturnAction],
  idiom: Idiom,
  // For a batch query, lifts other than the one from the primary liftQuery go here. THey need to be know about separately
  // in the batch query case. Should be empty & ignored for non batch cases.
  secondaryLifts: List[PlanterExpr[?, ?, ?]] = List()
)(queryAst: => Ast):
  /**
   * Plant all the lifts and return them. NOTE: If this is used frequently would
   * it be worth caching (i.e. since this object is immutable) and splicing them
   * might be expensive if it is done over and over again.
   */
  def lifts(using Quotes) = rawLifts.map(_.plant)
  def ast: Ast            = queryAst
