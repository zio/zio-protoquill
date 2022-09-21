package io.getquill

import io.getquill.util.Format

/**
 * TODO Not needed now since elaboration does not do OntoAst?
 */
enum OuterSelectWrap:
  case Always
  case Never
  case Default

object OuterSelectWrap:
  import scala.quoted._

  given ToExpr[OuterSelectWrap] with
    def apply(e: OuterSelectWrap)(using Quotes): Expr[OuterSelectWrap] =
      e match
        case OuterSelectWrap.Always  => '{ OuterSelectWrap.Always }
        case OuterSelectWrap.Never   => '{ OuterSelectWrap.Never }
        case OuterSelectWrap.Default => '{ OuterSelectWrap.Default }

  given FromExpr[OuterSelectWrap] with
    def unapply(e: Expr[OuterSelectWrap])(using Quotes): Option[OuterSelectWrap] =
      e match
        case '{ OuterSelectWrap.Always }  => Some(OuterSelectWrap.Always)
        case '{ OuterSelectWrap.Never }   => Some(OuterSelectWrap.Never)
        case '{ OuterSelectWrap.Default } => Some(OuterSelectWrap.Default)
        case _                            => None

  def lift(e: OuterSelectWrap)(using Quotes) = Expr(e)
  def unlift(e: Expr[OuterSelectWrap])(using Quotes) =
    import quotes.reflect._
    e match
      case Expr(expr) => expr
      case _ => report.throwError(
          s"""
        |Cannot unlift OuterSelectWrap from the value: ${Format.Expr(e)}.
        |The OuterSelectWrap parameter needs to be used as a constant for example:
        |run(query[Person].map(p => p.name), OuterSelectWrap.Never)
        """.stripMargin
        )
end OuterSelectWrap
