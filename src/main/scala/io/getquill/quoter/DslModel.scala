package io.getquill.quoter

import io.getquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import scala.deriving._
import scala.quoted.Const
import io.getquill.dsl.GenericEncoder
import io.getquill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly
import io.getquill.Query
import io.getquill.EntityQuery

// TODO lifts needs to be List of Planter to allow QueryLifts
case class Quoted[+T](val ast: io.getquill.ast.Ast, lifts: List[Planter[_, _]], runtimeQuotes: List[QuotationVase])
  //override def toString = ast.toString
  // make a function that uses a stateless transformer to walk through the tuple,
  // gather the lifted quoted blocks, splice their qutations into the ast, and then
  // add their lifted values into the parent tuple.... basically a runtime
  // flattening of the tree. This is the mechanism that will be used by the 'run' function
  // for dynamic queries




// Planters contain trees that can be re-inserted into compile-time code.
// For example, a ScalarPlanter is re-inserted into the PrepareRow sequence
//sealed trait Planter

sealed trait Planter[T, PrepareRow](value: T, uid: String) {
  def unquote: T
}

case class EagerPlanter[T, PrepareRow](value: T, encoder: GenericEncoder[T, PrepareRow], uid: String) extends Planter[T, PrepareRow](value, uid) {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}

case class LazyPlanter[T, PrepareRow](value: T, uid: String) extends Planter[T, PrepareRow](value, uid) {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}

// Stores runtime quotation tree. This is holder for quotations that are not inline thus can never be re-inserted into
// the ast (i.e. the ground... metaphorically speaking), therefore this holder is called Vase. The contents of the
// QuotationVase are only inserted back in during runtime.
case class QuotationVase(quoted: Quoted[Any], uid: String)

// Quotations go from a QuotationLot directly inline into the tree or put into a QuotationVase
// to be added into the runtime inlining later
trait QuotationLot[+T](quoted: Quoted[T], uid: String) {
  // TODO I think we should get rid of this operator. Unquote should be put on this via an implicit class which causes
  // invocation of the unquote macro?
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}

case class Unquote[+T](quoted: Quoted[T], uid: String) extends QuotationLot[T](quoted, uid)

// TODO Does this need to be covariant? It is in current quill. Need to look up what use cases they are for covariant schemas.
case class SchemaMeta[T](val entity: Quoted[io.getquill.EntityQuery[T]], uid: String) extends QuotationLot[EntityQuery[T]](entity, uid)

case class InsertMeta[T](val entity: Quoted[T], uid: String) extends QuotationLot[T](entity, uid)

case class QueryMeta[T, R](val entity: Quoted[Query[T] => Query[R]], uid: String, extract: R => T) extends QuotationLot[Query[T] => Query[R]](entity, uid)
