package io.getquill.parser.engine

sealed trait History:
  def name: String
  def withFailure(expr: String) = History.Failed(expr, this)

sealed trait HistoryPart extends History:
  def name = chain.name
  def chain: ParserChain

object History:
  case class Failed(expr: String, parent: History) extends History { val name = "Failed" }
  case class Matched(chain: ParserChain, parent: History)(expr: =>String) extends HistoryPart
  case class Ignored(chain: ParserChain, parent: History)(expr: =>String) extends HistoryPart
  case object Root extends History { val name = "Root" }