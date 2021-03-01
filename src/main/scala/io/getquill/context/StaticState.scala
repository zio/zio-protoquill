package io.getquill.context

import scala.quoted._
import io.getquill.Planter

case class StaticState(query: String, lifts: List[Expr[Planter[?, ?]]])