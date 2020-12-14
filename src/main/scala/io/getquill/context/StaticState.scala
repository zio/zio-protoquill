package io.getquill.context

import scala.quoted._

case class StaticState(query: String, lifts: Expr[List[miniquill.quoter.ScalarPlanter[?, ?]]])