package io.getquill.context

import scala.quoted._

case class StaticState(query: String, lifts: Expr[List[io.getquill.quoter.ScalarPlanter[?, ?]]])