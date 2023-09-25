package io.getquill.context

import scala.quoted._

sealed trait SplicingBehavior
object SplicingBehavior:
  sealed trait FailOnDynamic extends SplicingBehavior
  case object FailOnDynamic  extends FailOnDynamic
  sealed trait AllowDynamic  extends SplicingBehavior
  case object AllowDynamic   extends AllowDynamic

trait SplicingBehaviorHint:
  type BehaviorType <: SplicingBehavior

object HasDynamicSplicingHint:
  object InExpr:
    def unapply(value: Expr[SplicingBehaviorHint])(using Quotes): Boolean =
      import quotes.reflect._
      val memberSymbol = value.asTerm.tpe.termSymbol.memberType("BehaviorType")
      value.asTerm.select(memberSymbol).tpe <:< TypeRepr.of[SplicingBehavior.FailOnDynamic]

  def fail(using Quotes): Boolean =
    import quotes.reflect._
    Expr.summon[SplicingBehaviorHint] match
      case Some(HasDynamicSplicingHint.InExpr()) =>
        true
      case Some(value) =>
        false
      case _ =>
        false
