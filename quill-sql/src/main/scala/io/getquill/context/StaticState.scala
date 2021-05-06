package io.getquill.context

import scala.quoted._
import io.getquill.Planter
import io.getquill.ReturnAction

case class StaticState(query: UnparticularQuery, lifts: List[Expr[Planter[?, ?]]], returnAction: Option[ReturnAction])