package io.getquill

import io.getquill.ast.Ast
import io.getquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import io.getquill.util.printer.AstPrinter
import scala.deriving._
import io.getquill.generic.GenericEncoder
import io.getquill.parser.ParserFactory
import io.getquill.parser.Parser.Implicits._
import io.getquill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly
import scala.compiletime.summonFrom
import io.getquill.EntityQuery
import io.getquill.Query
import io.getquill.Insert
import io.getquill.context.InsertMacro
import io.getquill.context.InsertMetaMacro
import io.getquill.context.SchemaMetaMacro
import io.getquill.context.QueryMetaMacro
import io.getquill.context.QueryMacro
import io.getquill.context.QuoteMacro
import io.getquill.context.UnquoteMacro
import io.getquill.context.LiftMacro
import io.getquill._

// trait Quoter {
//   def quote[T](bodyExpr: Quoted[T]): Quoted[T] = ???
//   def quote[T](bodyExpr: T): Quoted[T] = ???
// }

// allows you to write quote, query, insert/delete/update, lazyLift with just importing this
// object lib extends Dsl[ParserLibrary] {
//   export io.getquill.Query
//   export io.getquill.Action
//   export io.getquill.Insert
//   export io.getquill.Update
//   export io.getquill.Delete
//   export io.getquill.ActionReturning
// }


//export io.getquill.Dsl._
//export io.getquill.Model._

object Dsl extends Dsl[ParserLibrary] // BaseParserFactory.type doesn't seem to work with the LoadObject used in quoteImpl

trait Dsl[Parser <: ParserFactory] extends QuoteDsl[Parser] with QueryDsl[Parser] with MetaDsl[Parser]

trait MetaDsl[Parser <: ParserFactory] extends QueryDsl[Parser] {

  inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] = 
    ${ SchemaMetaMacro[T, Parser]('this, 'entity, 'columns) }

  inline def queryMeta[T, R](inline expand: Quoted[Query[T] => Query[R]])(inline extract: R => T): QueryMeta[T, R] =
    ${ QueryMetaMacro.embed[T, R, Parser]('this, 'expand, 'extract) }

  /** Automatic implicit ordering DSL for: `query[Person].sortBy(_.field)(<here>)` */
  implicit def implicitOrd[T]: Ord[T] = Ord.ascNullsFirst
}

object QueryDsl {
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

  inline def insertMeta[T](inline exclude: (T => Any)*): InsertMeta[T] = ${ InsertMetaMacro[T, Parser]('exclude) }

  inline def lazyLift[T](inline vv: T): T = ${ LiftMacro.applyLazy[T, Nothing]('vv) }

  inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ UnquoteMacro[T]('quoted) }

  inline implicit def autoQuote[T](inline body: T): Quoted[T] = ${ QuoteMacro[T, Parser]('body) }

  extension [T](inline entity: EntityQuery[T])
    // Note that although this is in the static DSL if you lift a case class inside the insert or anything else, it will try to do a standard lift for that
    // requiring a context to be present
    inline def insert(inline value: T): Insert[T] = ${ InsertMacro[T, Parser]('entity, 'value) }
}
