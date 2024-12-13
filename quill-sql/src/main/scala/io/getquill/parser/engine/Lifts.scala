package io.getquill.parser.engine

import scala.collection.mutable.ListBuffer
import scala.quoted.Expr
import io.getquill.{ Planter, QuotationVase }

class Lifts private (
  private val underlyingLifts: ListBuffer[Expr[Planter[?, ?, ?]]],
  private val underlyingPluckableUnquotes: ListBuffer[Expr[QuotationVase]],
) {
  def addLift(lift: Expr[Planter[?, ?, ?]]) =
    underlyingLifts.addOne(lift)
  def addLifts(lifts: List[Expr[Planter[?, ?, ?]]]) =
    underlyingLifts.addAll(lifts)
  def addPluckableUnquote(vase: Expr[QuotationVase]) =
    underlyingPluckableUnquotes.addOne(vase)

  def lifts = underlyingLifts.toList
  def pluckableUnquotes = underlyingPluckableUnquotes.toList
}

object Lifts {
  def Empty = new Lifts(ListBuffer.empty, ListBuffer.empty)
}
