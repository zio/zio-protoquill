package miniquill.quoter

import io.getquill.ast.Ast
import miniquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import derivation._
import scala.deriving._
import scala.quoted.matching.Const
import miniquill.dsl.GenericEncoder

object QuoteDsl {



  def parserFactory: (QuoteContext) => PartialFunction[Expr[_], Ast] = 
    (qctx: QuoteContext) => new Parser(given qctx)

  def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: QuoteContext) => new Lifter(given qctx)

  inline def query[T]: EntityQuery[T] = new EntityQuery

  inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ quoteImpl[T]('bodyExpr) }

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ quoteImpl[T]('bodyExpr) }

  def quoteImpl[T: Type](bodyRaw: Expr[T])(given qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given _}
    // NOTE Can disable if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = bodyRaw.unseal.underlyingArgument.seal

    // TODo add an error if body cannot be parsed
    val ast = parserFactory(qctx).apply(body)

    println("Ast Is: " + ast)

    // TODO Add an error if the lifting cannot be found
    val reifiedAst = lifterFactory(qctx)(ast)

    val pluckedUnquotes = extractRuntimeUnquotes(body)

    // Extract new lifts
    val lifts = extractLifts(body)

    // TODO Extract ScalarPlanter which are lifts that have been transformed already
    // TODO Extract plucked quotations, transform into QuotationVase statements and insert into runtimeQuotations slot

    '{       
      Quoted[T](${reifiedAst}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)})
    }
  }

  def extractLifts(body: Expr[Any])(given qctx: QuoteContext) = {
    ScalarPlanterExpr.findUnquotes(body).map(_.plant)
  }

  def extractRuntimeUnquotes(body: Expr[Any])(given qctx: QuoteContext) = {
    val unquotes = QuotationBinExpr.findUnquotes(body)
    unquotes
      .collect { case expr: PluckedQuotationBinExpr => expr }
      .map(_.toVaseExpr)
  }


  def runQuery[T](query: Quoted[Query[T]]): String = ???

  def run[T](query: Quoted[T]): String = {
    query.ast.toString
  }

  import scala.language.implicitConversions

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T: Type](quoted: Expr[Quoted[T]])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    '{
      QuotationBin[T](${quoted}, ${Expr(java.util.UUID.randomUUID().toString)}).unquote
    }
  }

  def querySchema[T](entity: String): EntityQuery[T] = ???

}
