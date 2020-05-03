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
import miniquill.parser.Parser.Implicits._
import io.getquill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly
import scala.compiletime.summonFrom

// trait Quoter {
//   def quote[T](bodyExpr: Quoted[T]): Quoted[T] = ???
//   def quote[T](bodyExpr: T): Quoted[T] = ???
// }

object QuoteDsl extends QuoteMeta[ParserLibrary] // BaseParserFactory.type doesn't seem to work with the LoadObject used in quoteImpl

class QuoteMeta[Parser <: ParserFactory] {

  //@compileTimeOnly(NonQuotedException.message)
  def querySchema[T](entity: String, columns: (T => (Any, String))*): EntityQuery[T] = NonQuotedException()

  inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] = 
    //SchemaMeta(quote { querySchema[T](entity, columns: _*) }, "1234") // TODO Don't need to generate a UID here.It can be static.
    ${ SchemaMetaMacro[T, Parser]('this, 'entity, 'columns) }
    

  inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  inline def query[T]: EntityQuery[T] = ${ QueryMacro[T] }

  def runQuery[T](query: Quoted[Query[T]]): String = ???

  def run[T](query: Quoted[T]): String = {
    query.ast.toString
  }

  import scala.language.implicitConversions

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ UnquoteMacro[T]('quoted) }

  inline implicit def autoQuote[T](inline body: T): Quoted[T] = ${ QuoteMacro[T, Parser]('body) }
}


object QuoteMacro {
  import io.getquill.util.LoadObject  

  def apply[T, Parser <: ParserFactory](bodyRaw: Expr[T])(given qctx: QuoteContext, tType: Type[T], pType: Type[Parser]): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given _}
    // NOTE Can disable if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = bodyRaw.unseal.underlyingArgument.seal

    val parserFactory = LoadObject(pType).get

    import Parser.{given _}

    // TODo add an error if body cannot be parsed
    val ast = parserFactory.apply(given qctx).seal.apply(body)

    println("Ast Is: " + ast)

    // TODO Add an error if the lifting cannot be found
    val reifiedAst = Lifter(qctx)(ast)

    val pluckedUnquotes = extractRuntimeUnquotes(body)

    // Extract new lifts
    val lifts = extractLifts(body)

    // TODO Extract ScalarPlanter which are lifts that have been transformed already
    // TODO Extract plucked quotations, transform into QuotationVase statements and insert into runtimeQuotations slot

    '{       
      Quoted[T](${reifiedAst}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)})
    }
  }

  // Find all lifts, dedupe by UID since lifts can be inlined multiple times hence
  // appearing in the AST in multiple places.
  private def extractLifts(body: Expr[Any])(given qctx: QuoteContext) = {
    ScalarPlanterExpr.findUnquotes(body).distinctBy(_.uid).map(_.plant)
  }

  private def extractRuntimeUnquotes(body: Expr[Any])(given qctx: QuoteContext) = {
    val unquotes = QuotationBinExpr.findUnquotes(body)
    unquotes
      .collect { case expr: PluckableQuotationBinExpr => expr }
      .distinctBy(_.uid)
      .map(_.pluck)
  }
}

object SchemaMetaMacro {
  import io.getquill.util.LoadObject

  // inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] = 
  // SchemaMeta(quote { querySchema[T](entity, columns: _*) }, "1234") // TODO Don't need to generate a UID here.It can be static.
  def apply[T, P <: ParserFactory](qm: Expr[QuoteMeta[P]], entity: Expr[String], columns: Expr[Seq[(T => (Any, String))]])(given qctx: QuoteContext, tType: Type[T], pType: Type[P]): Expr[SchemaMeta[T]] = {
    val parserFactory = LoadObject(pType).get
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    import scala.quoted.matching.ExprSeq
    val exprs = 
      (columns match {
        case ExprSeq(argsExprs) => argsExprs
      }).toList
    //val quote = quoteImpl('{ $qm.querySchema[T]($entity, ${Expr.ofList(exprs)}: _*) })
    val quote = QuoteMacro('{ $qm.querySchema[T]($entity, $columns: _*) })
    '{ SchemaMeta($quote, $uuid) }
  }
}


object QueryMacro {
  def apply[T: Type](given qctx: QuoteContext): Expr[EntityQuery[T]] = {
    import qctx.tasty.{given, _}
    import scala.quoted.matching.summonExpr

    summonExpr(given '[SchemaMeta[T]]) match {
      case Some(meta) =>
        '{ $meta.unquote }

      case None => 
        '{ new EntityQuery[T]() }
    }
  }
}


object UnquoteMacro {
  def apply[T: Type](quoted: Expr[Quoted[T]])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    '{
      Unquote[T](${quoted}, ${Expr(java.util.UUID.randomUUID().toString)}).unquote
    }
  }

}
