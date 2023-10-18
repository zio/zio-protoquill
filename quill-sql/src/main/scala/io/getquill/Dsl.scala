package io.getquill

import io.getquill.ast.Ast
import io.getquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
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
import io.getquill.context.StaticSpliceMacro
import scala.language.implicitConversions
import io.getquill.dsl.DateOps

implicit val defaultParser: ParserLibrary = ParserLibrary

inline def schemaMeta[T](inline entity: String, inline columns: (T => (Any, String))*): SchemaMeta[T] =
  ${ SchemaMetaMacro[T]('entity, 'columns) }

inline def queryMeta[T, R](inline expand: Quoted[Query[T] => Query[R]])(inline extract: R => T): QueryMeta[T, R] =
  ${ QueryMetaMacro.embed[T, R]('expand, 'extract) }

/**
 * Automatic implicit ordering DSL for: `query[Person].sortBy(_.field)(<here>)`
 */
implicit def implicitOrd[T]: Ord[T] = Ord.ascNullsFirst

extension (str: String) {
  def like(other: String): Boolean = ???
}

inline def query[T]: EntityQuery[T] = ${ QueryMacro[T] }
inline def select[T]: Query[T]      = ${ QueryMacro[T] }

def max[A](a: A): A                                  = NonQuotedException()
def min[A](a: A): A                                  = NonQuotedException()
def count[A](a: A): A                                = NonQuotedException()
def avg[A](a: A)(implicit n: Numeric[A]): BigDecimal = NonQuotedException()
def sum[A](a: A)(implicit n: Numeric[A]): A          = NonQuotedException()

def avg[A](a: Option[A])(implicit n: Numeric[A]): Option[BigDecimal] = NonQuotedException()
def sum[A](a: Option[A])(implicit n: Numeric[A]): Option[A]          = NonQuotedException()

extension [T](o: Option[T]) {
  def filterIfDefined(f: T => Boolean): Boolean = NonQuotedException()
}

object extras extends DateOps {
  extension [T](a: T) {
    def getOrNull: T =
      throw new IllegalArgumentException(
        "Cannot use getOrNull outside of database queries since only database value-types (e.g. Int, Double, etc...) can be null."
      )

    def ===(b: T): Boolean =
      (a, b) match {
        case (a: Option[_], b: Option[_]) => a.exists(av => b.exists(bv => av == bv))
        case (a: Option[_], b)            => a.exists(av => av == b)
        case (a, b: Option[_])            => b.exists(bv => bv == a)
        case (a, b)                       => a == b
      }

    def =!=(b: T): Boolean =
      (a, b) match {
        case (a: Option[_], b: Option[_]) => a.exists(av => b.exists(bv => av != bv))
        case (a: Option[_], b)            => a.exists(av => av != b)
        case (a, b: Option[_])            => b.exists(bv => bv != a)
        case (a, b)                       => a != b
      }
  }
}

inline def static[T](inline value: T): T = ${ StaticSpliceMacro('value) }

inline def insertMeta[T](inline exclude: (T => Any)*): InsertMeta[T] = ${ InsertMetaMacro[T]('exclude) }

inline def updateMeta[T](inline exclude: (T => Any)*): UpdateMeta[T] = ${ UpdateMetaMacro[T]('exclude) }

inline def lazyLift[T](inline vv: T): T = ${ LiftMacro.applyLazy[T, Nothing]('vv) }

inline def quote[T](inline bodyExpr: Quoted[T]): Quoted[T] = ${ QuoteMacro[T]('bodyExpr) }

inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ QuoteMacro[T]('bodyExpr) }

inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ UnquoteMacro[T]('quoted) }

inline implicit def autoQuote[T](inline body: T): Quoted[T] = ${ QuoteMacro[T]('body) }

// Doing:          val p = quote { query[Person] }
// and then doing: val q = quote { p.insert(_.name -> "blah") }
//  or then doing: val q = quote { p.insertValue(lift(Person("Joe", 123))) }
// confuses Dotty since it needs to be `p.unquote` first and it can't determine which
// variant of the function it is supposed to use. Therefore we have to explicitly define
// these functions on the quoted variant of the EntityQuery for the types to infer correctly.
// see ActionSpec.scala action->insert->simple, using nested select, etc... tets for examples of this
extension [T](inline quotedEntity: Quoted[EntityQuery[T]]) {
  inline def insert(inline f: (T => (Any, Any)), inline f2: (T => (Any, Any))*): Insert[T] =
    unquote[EntityQuery[T]](quotedEntity).insert(f, f2: _*)
  inline def update(inline f: (T => (Any, Any)), inline f2: (T => (Any, Any))*): Update[T] =
    unquote[EntityQuery[T]](quotedEntity).update(f, f2: _*)
}
