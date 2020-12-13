package miniquill.quoter

import io.getquill.ast.Ast
import miniquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import derivation._
import scala.deriving._
import scala.quoted.Const
import miniquill.dsl.GenericEncoder
import miniquill.parser.ParserFactory
import miniquill.parser.Parser.Implicits._
import io.getquill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly
import scala.compiletime.summonFrom
import io.getquill.EntityQuery
import io.getquill.Query

// trait Quoter {
//   def quote[T](bodyExpr: Quoted[T]): Quoted[T] = ???
//   def quote[T](bodyExpr: T): Quoted[T] = ???
// }

object Dsl extends Dsl[ParserLibrary] // BaseParserFactory.type doesn't seem to work with the LoadObject used in quoteImpl


trait Dsl[Parser <: ParserFactory] 
extends QuoteDsl[Parser] 
with QueryDsl[Parser] 
with MetaDsl[Parser] {

  def run[T](query: Quoted[T]): String = {
    query.ast.toString
  }
}

trait MetaDsl[Parser <: ParserFactory] extends QueryDsl[Parser] {
  @compileTimeOnly(NonQuotedException.message)
  def querySchema[T](entity: String, columns: (T => (Any, String))*): EntityQuery[T] = NonQuotedException()

  inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] = 
    ${ SchemaMetaMacro[T, Parser]('this, 'entity, 'columns) }

  inline def queryMeta[T, R](inline expand: Quoted[Query[T] => Query[R]])(inline extract: R => T): QueryMeta[T, R] =
    ${ QueryMetaMacro.embed[T, R, Parser]('this, 'expand, 'extract) }
}


object QueryMetaMacro {
  def embed[T: Type, R: Type, P <: ParserFactory: Type](qm: Expr[MetaDsl[P]], expand: Expr[Quoted[Query[T] => Query[R]]], extract: Expr[R => T])(using Quotes): Expr[QueryMeta[T, R]] = {
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    '{ QueryMeta[T, R]($expand, $uuid, $extract) }
  }

  // def extractApply[T: Type, R: Type](qm: Expr[QueryMeta[T, R]], qry: Quoted[Query[T]])(using Quotes): Option[Expr[Quoted[Query[R]]]] = {
  //   qm match {
  //     ``
  //   }
  // }
}

object QueryDsl {
  // implicit class StringExt(str: String) {
  //   def like(str: String): Boolean = ???
  // }
  extension (str: String) {
    def like(other: String): Boolean = ???
  }
}

trait QueryDsl[Parser <: ParserFactory] {
  inline def query[T]: EntityQuery[T] = ${ QueryMacro[T] }
  inline def select[T]: Query[T] = ${ QueryMacro[T] }
}

trait QuoteDsl[Parser <: ParserFactory] {
  import scala.language.implicitConversions

  inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  inline def myquote[T](inline bodyExpr: T): Quoted[T] = ${ MyQuoteMacro[T, Parser]('bodyExpr) }

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ UnquoteMacro[T]('quoted) }

  inline implicit def autoQuote[T](inline body: T): Quoted[T] = ${ QuoteMacro[T, Parser]('body) }
}


object MyQuoteMacro {
  import io.getquill.util.LoadObject 
  import io.getquill.norm.BetaReduction 

  def apply[T, Parser <: ParserFactory](bodyRaw: Expr[T])(using Quotes, Type[T], Type[Parser]): Expr[Quoted[T]] = {
    import quotes.reflect._
    // NOTE Can disable if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = Term.of(bodyRaw).underlyingArgument.asExpr

    val parserFactory = LoadObject[Parser].get

    import Parser._

    val pluckedUnquotes = extractRuntimeUnquotes(bodyRaw)

    // Extract new lifts
    val lifts = extractLifts(bodyRaw)

    // TODO Extract ScalarPlanter which are lifts that have been transformed already
    // TODO Extract plucked quotations, transform into QuotationVase statements and insert into runtimeQuotations slot

    // ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}
    '{       
      Quoted[T](io.getquill.ast.Ident("p"), ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)})
    }
  }

  // Find all lifts, dedupe by UID since lifts can be inlined multiple times hence
  // appearing in the AST in multiple places.
  private def extractLifts(body: Expr[Any])(using Quotes) = {
    ScalarPlanterExpr.findUnquotes(body).distinctBy(_.uid).map(_.plant)
  }

  private def extractRuntimeUnquotes(body: Expr[Any])(using Quotes) = {
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
  import io.getquill.util.LoadObject 
  import io.getquill.norm.BetaReduction 

  def apply[T, Parser <: ParserFactory](bodyRaw: Expr[T])(using Quotes, Type[T], Type[Parser]): Expr[Quoted[T]] = {
    import quotes.reflect._
    // NOTE Can disable if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = Term.of(bodyRaw).underlyingArgument.asExpr

    val parserFactory = LoadObject[Parser].get

    import Parser._

    val pluckedUnquotes = extractRuntimeUnquotes(bodyRaw)

    // Extract new lifts
    val lifts = extractLifts(bodyRaw)

    // TODO Extract ScalarPlanter which are lifts that have been transformed already
    // TODO Extract plucked quotations, transform into QuotationVase statements and insert into runtimeQuotations slot

    // ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}
    '{       
      Quoted[T](io.getquill.ast.Ident("p"), ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)})
    }
  }

  // Find all lifts, dedupe by UID since lifts can be inlined multiple times hence
  // appearing in the AST in multiple places.
  private def extractLifts(body: Expr[Any])(using Quotes) = {
    ScalarPlanterExpr.findUnquotes(body).distinctBy(_.uid).map(_.plant)
  }

  private def extractRuntimeUnquotes(body: Expr[Any])(using Quotes) = {
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


object SchemaMetaMacro {
  import io.getquill.util.LoadObject

  // inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] = 
  // SchemaMeta(quote { querySchema[T](entity, columns: _*) }, "1234") // TODO Don't need to generate a UID here.It can be static.
  def apply[T, P <: ParserFactory](qm: Expr[MetaDsl[P]], entity: Expr[String], columns: Expr[Seq[(T => (Any, String))]])(using Quotes, Type[T], Type[P]): Expr[SchemaMeta[T]] = {
    val tmc = new TastyMatchersContext
    import tmc._
    val parserFactory = LoadObject[P].get
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    val exprs = 
      (columns match {
        case GenericSeq(argsExprs) => argsExprs
      }).toList
    //val quote = quoteImpl('{ $qm.querySchema[T]($entity, ${Expr.ofList(exprs)}: _*) })
    val quote = QuoteMacro('{ $qm.querySchema[T]($entity, $columns: _*) })
    '{ SchemaMeta($quote, $uuid) }
  }
}


object QueryMacro {
  def apply[T: Type](using Quotes): Expr[EntityQuery[T]] = {
    import quotes.reflect._
    val tmc = new TastyMatchersContext
    import tmc._
    import scala.quoted.Expr.summon
    import miniquill.quoter.QuotationLotExpr
    import miniquill.quoter.QuotationLotExpr._


    Expr.summon[SchemaMeta[T]] match {
      case Some(meta) =>
        meta.reseal match {
          // If it is uprootable, unquote the meta and pass it on
          case QuotationLotExpr(Uprootable(_, _, _, _, _, _)) =>
            '{ $meta.unquote }

          // If it's pluckabke can also return that because the parser/Expr accumulate in Context will find it.
          // I am not sure this has use cases.
          case QuotationLotExpr(Pluckable(_, _, _)) =>
            '{ $meta.unquote }
              
          // In case it's only pointable, need to synthesize a new UID for the quotation
          case QuotationLotExpr(Pointable(_)) => //hello
            UnquoteMacro('{$meta.entity})

          case _ => report.throwError("Invalid Quotation:\n" + meta.show, meta)
        }

      case None => 
        '{ new EntityQuery[T]() }
    }
  }
}


object UnquoteMacro {
  def apply[T: Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = {
    import quotes.reflect._
    '{
      Unquote[T](${quoted}, ${Expr(java.util.UUID.randomUUID().toString)}).unquote
    }
  }

}
