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
import io.getquill.Insert
import io.getquill.context.InsertMacro
import io.getquill.InsertMetaMacro

// trait Quoter {
//   def quote[T](bodyExpr: Quoted[T]): Quoted[T] = ???
//   def quote[T](bodyExpr: T): Quoted[T] = ???
// }

object Dsl extends Dsl[ParserLibrary] // BaseParserFactory.type doesn't seem to work with the LoadObject used in quoteImpl


trait Dsl[Parser <: ParserFactory] extends QuoteDsl[Parser] with QueryDsl[Parser] with MetaDsl[Parser] {
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

  inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ QuoteMacro[T, Parser]('bodyExpr) }

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ UnquoteMacro[T]('quoted) }

  inline implicit def autoQuote[T](inline body: T): Quoted[T] = ${ QuoteMacro[T, Parser]('body) }

  extension [T](inline entity: EntityQuery[T])
    inline def insertI(inline value: T): Insert[T] = ${ InsertMacro.apply[T, Parser]('entity, 'value) }
}
