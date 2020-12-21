package miniquill.quoter

import scala.quoted._
import miniquill.parser.ParserFactory
import io.getquill.util.LoadObject 
import io.getquill.norm.BetaReduction
import miniquill.parser.Parser
import miniquill.parser.Parser.Implicits._
import miniquill.parser.Lifter

object LiftExtractHelper {
  // Find all lifts, dedupe by UID since lifts can be inlined multiple times hence
  // appearing in the AST in multiple places.
  def extractLifts(body: Expr[Any])(using Quotes) = {
    ScalarPlanterExpr.findUnquotes(body).distinctBy(_.uid).map(_.plant)
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
    val pluckedUnquotes = LiftExtractHelper.extractRuntimeUnquotes(bodyRaw)
    val lifts = LiftExtractHelper.extractLifts(bodyRaw)

    // TODO Extract ScalarPlanter which are lifts that have been transformed already
    // TODO Extract plucked quotations, transform into QuotationVase statements and insert into runtimeQuotations slot

    // ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}
    '{ Quoted[T](${reifiedAst}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}) }
  }
}
