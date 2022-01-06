package io.getquill

import io.getquill.ast.Ast
import io.getquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import io.getquill.util.printer.AstPrinter
import scala.deriving._
import io.getquill.generic.GenericEncoder
import io.getquill.parser.ParserFactory
import io.getquill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly
import scala.compiletime.summonFrom
import io.getquill.EntityQuery
import io.getquill.Query
import io.getquill.Insert
import io.getquill.context.InsertUpdateMacro
import io.getquill.context.InsertMetaMacro
import io.getquill.context.UpdateMetaMacro
import io.getquill.context.SchemaMetaMacro
import io.getquill.context.QueryMetaMacro
import io.getquill.context.QueryMacro
import io.getquill.context.QuoteMacro
import io.getquill.context.UnquoteMacro
import io.getquill.context.LiftMacro
import io.getquill._
import io.getquill.dsl.InfixDsl
import io.getquill.context.StaticSpliceMacro

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
implicit val defaultParser: ParserLibrary = ParserLibrary

object Dsl extends Dsl // BaseParserFactory.type doesn't seem to work with the LoadModule used in quoteImpl

trait Dsl extends QuoteDsl with QueryDsl with MetaDsl

trait MetaDsl extends QueryDsl {

  inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] =
    ${ SchemaMetaMacro[T]('this, 'entity, 'columns) }

  inline def queryMeta[T, R](inline expand: Quoted[Query[T] => Query[R]])(inline extract: R => T): QueryMeta[T, R] =
    ${ QueryMetaMacro.embed[T, R]('this, 'expand, 'extract) }

  /** Automatic implicit ordering DSL for: `query[Person].sortBy(_.field)(<here>)` */
  implicit def implicitOrd[T]: Ord[T] = Ord.ascNullsFirst
}

object QueryDsl {
  extension (str: String) {
    def like(other: String): Boolean = ???
  }
}

trait QueryDsl {
  inline def query[T]: EntityQuery[T] = ${ QueryMacro[T] }
  inline def select[T]: Query[T] = ${ QueryMacro[T] }
}

trait QuoteDsl {
  import scala.language.implicitConversions

  inline def static[T](inline value: T): T = ${ StaticSpliceMacro('value) }

  inline def insertMeta[T](inline exclude: (T => Any)*): InsertMeta[T] = ${ InsertMetaMacro[T]('exclude) }

  inline def updateMeta[T](inline exclude: (T => Any)*): UpdateMeta[T] = ${ UpdateMetaMacro[T]('exclude) }

  inline def lazyLift[T](inline vv: T): T = ${ LiftMacro.applyLazy[T, Nothing]('vv) }

  inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ QuoteMacro[T]('bodyExpr) }

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ QuoteMacro[T]('bodyExpr) }

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ UnquoteMacro[T]('quoted) }

  inline implicit def autoQuote[T](inline body: T): Quoted[T] = ${ QuoteMacro[T]('body) }

  extension [T](inline entity: EntityQuery[T])
    // Note that although this is in the static DSL if you lift a case class inside the insert or anything else, it will try to do a standard lift for that
    // requiring a context to be present
    inline def insert(inline value: T): Insert[T] = ${ InsertUpdateMacro[T, Insert]('entity, 'value) }
    inline def update(inline value: T): Update[T] = ${ InsertUpdateMacro[T, Update]('entity, 'value) }

  // Doing:          val p = quote { query[Person] }
  // and then doing: val q = quote { p.insert(_.name -> "blah") }
  //  or then doing: val q = quote { p.insert(lift(Person("Joe", 123))) }
  // confuses Dotty since it needs to be `p.unquote` first and it can't determine which
  // variant of the function it is supposed to use. Therefore we have to explicitly define
  // these functions on the quoted variant of the EntityQuery for the types to infer correctly.
  // see ActionSpec.scala action->insert->simple, using nested select, etc... tets for examples of this
  extension [T](inline quotedEntity: Quoted[EntityQuery[T]])
    inline def insert(inline f: (T => (Any, Any)), inline f2: (T => (Any, Any))*): Insert[T] = unquote[EntityQuery[T]](quotedEntity).insert(f, f2: _*)
    inline def update(inline f: (T => (Any, Any)), inline f2: (T => (Any, Any))*): Update[T] = unquote[EntityQuery[T]](quotedEntity).update(f, f2: _*)
    inline def insert(inline value: T): Insert[T] = unquote[EntityQuery[T]](quotedEntity).insert(value)
    inline def update(inline value: T): Update[T] = unquote[EntityQuery[T]](quotedEntity).update(value)
}
