package io.getquill.context

import scala.quoted._
import io.getquill.parser.ParserFactory
import io.getquill.util.LoadObject 
import io.getquill.norm.BetaReduction
import io.getquill.parser.Parser
import io.getquill.parser.Parser.Implicits._
import io.getquill.parser.Lifter
import io.getquill.quoter.PlanterExpr
import io.getquill.quoter.QuotationLotExpr
import io.getquill.quoter.Pluckable
import io.getquill.quoter.Pointable
import io.getquill.quoter.Quoted


object ExtractLifts {
  // Find all lifts, dedupe by UID since lifts can be inlined multiple times hence
  // appearing in the AST in multiple places.
  def extractLifts(body: Expr[Any])(using Quotes) = {
    // TODO If we want to support functionality to resolve all LazyPlanters into
    // eager Planters before the 'run' function, need to look thorugh eager/lazy
    // (planters) with the same UID and choose the ones that are eager for the same UID
    // i.e. since they would be the resolved ones
    PlanterExpr.findUnquotes(body).distinctBy(_.uid).map(_.plant)
  }

  def extractRuntimeUnquotes(body: Expr[Any])(using Quotes) = {
    import quotes.reflect.report
    val unquotes = QuotationLotExpr.findUnquotes(body)
    unquotes
      .collect {
        case expr: Pluckable => expr
        case Pointable(expr) =>
          report.throwError(s"Invalid runtime Quotation: ${expr.show}. Cannot extract a unique identifier.", expr)
      }
      .distinctBy(_.uid)
      .map(_.pluck)
  }

  def apply(body: Expr[Any])(using Quotes) =
    (extractLifts(body), extractRuntimeUnquotes(body))
}

object QuoteMacro {

  def apply[T, Parser <: ParserFactory](bodyRaw: Expr[T])(using Quotes, Type[T], Type[Parser]): Expr[Quoted[T]] = {
    import quotes.reflect._
    // NOTE Can disable if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = bodyRaw.asTerm.underlyingArgument.asExpr

    val parserFactory = LoadObject[Parser].get

    import Parser._

    // TODo add an error if body cannot be parsed
    val rawAst = parserFactory.apply.seal.apply(body)
    val ast = BetaReduction(rawAst)

    //println("Ast Is: " + ast)

    // TODO Add an error if the lifting cannot be found
    val reifiedAst = Lifter(ast)

    //println("========= AST =========\n" + io.getquill.util.Messages.qprint(ast))

    // Extract runtime quotes and lifts
    val (lifts, pluckedUnquotes) = ExtractLifts(bodyRaw)

    // TODO Extract Planter which are lifts that have been transformed already
    // TODO Extract plucked quotations, transform into QuotationVase statements and insert into runtimeQuotations slot

    // ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}
    '{ Quoted[T](${reifiedAst}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}) }
  }
}
