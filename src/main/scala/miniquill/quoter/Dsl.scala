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
import miniquill.parser.ParserFactory
import io.getquill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly

// trait Quoter {
//   def quote[T](bodyExpr: Quoted[T]): Quoted[T] = ???
//   def quote[T](bodyExpr: T): Quoted[T] = ???
// }

object QuoteDsl extends QuoteMeta[ParserLibrary] // BaseParserFactory.type doesn't seem to work with the LoadObject used in quoteImpl

class QuoteMeta[P <: ParserFactory] {

  //@compileTimeOnly(NonQuotedException.message)
  def querySchema[T](entity: String, columns: (T => (Any, String))*): EntityQuery[T] = NonQuotedException()

  inline def schemaMeta[T](entity: String, columns: (T => (Any, String))*): SchemaMeta[T] = 
    SchemaMeta(quote { querySchema[T](entity, columns: _*) })

  inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ QuoteImpl.quoteImpl[T, P]('bodyExpr) }

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ QuoteImpl.quoteImpl[T, P]('bodyExpr) }

  inline def query[T]: EntityQuery[T] = new EntityQuery()

  def runQuery[T](query: Quoted[Query[T]]): String = ???

  def run[T](query: Quoted[T]): String = {
    query.ast.toString
  }

  import scala.language.implicitConversions

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ QuoteImpl.unquoteImpl[T]('quoted) }

  inline implicit def autoQuote[T](inline body: T): Quoted[T] = ${ QuoteImpl.quoteImpl[T, P]('body) }
}

object QuoteImpl {
  import io.getquill.util.LoadObject

  // def parserFactory: (QuoteContext) => Parser = 
  //   (qctx: QuoteContext) => new BaseParser(given qctx).parser

  def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: QuoteContext) => new Lifter(given qctx)

  def quoteImpl[T, P <: ParserFactory](bodyRaw: Expr[T])(given qctx: QuoteContext, tType: Type[T], pType: Type[P]): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given _}
    // NOTE Can disable if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = bodyRaw.unseal.underlyingArgument.seal

    val parserFactory = LoadObject(pType).get

    // TODo add an error if body cannot be parsed
    val ast = parserFactory.apply(given qctx).apply(body)

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

  def unquoteImpl[T: Type](quoted: Expr[Quoted[T]])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    '{
      QuotationBin[T](${quoted}, ${Expr(java.util.UUID.randomUUID().toString)}).unquote
    }
  }

  private def extractLifts(body: Expr[Any])(given qctx: QuoteContext) = {
    ScalarPlanterExpr.findUnquotes(body).map(_.plant)
  }

  private def extractRuntimeUnquotes(body: Expr[Any])(given qctx: QuoteContext) = {
    val unquotes = QuotationBinExpr.findUnquotes(body)
    unquotes
      .collect { case expr: PluckableQuotationBinExpr => expr }
      .map(_.pluck)
  }
}
