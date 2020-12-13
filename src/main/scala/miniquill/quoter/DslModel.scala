package miniquill.quoter

import miniquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import derivation._
import scala.deriving._
import scala.quoted.Const
import miniquill.dsl.GenericEncoder
import io.getquill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly
import io.getquill.Query
import io.getquill.EntityQuery


case class Quoted[+T](val ast: io.getquill.ast.Ast, lifts: List[ScalarPlanter[_, _]], runtimeQuotes: List[QuotationVase])

case class MyQuoted(val ast: String, runtimeQuotes: List[MyQuotationVase])
case class MyQuotationVase(str: String)


// Planters contain trees that can be re-inserted into compile-time code.
// For example, a ScalarPlaner is re-inserted into the PrepareRow sequence
//sealed trait Planter

case class ScalarPlanter[T, PrepareRow](value: T, encoder: GenericEncoder[T, PrepareRow], uid: String) {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}

// Stores runtime quotation tree. This is holder for quotations that are not inline thus can never be re-inserted into
// the ast (i.e. the ground... metaphorically speaking), therefore this holder is called Vase. The contents of the
// QuotationVase are only inserted back in during runtime.
case class QuotationVase(quoted: Quoted[Any], uid: String)



// Quotations go from a QuotationLot directly inline into the tree or put into a QuotationVase
// to be added into the runtime inlining later
trait QuotationLot[+T](val quoted: Quoted[T], val uid: String) {
  // TODO I think we should get rid of this operator. Unquote should be put on this via an implicit class which causes
  // invocation of the unquote macro?
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}

case class Unquote[+T](override val quoted: Quoted[T], override val uid: String) extends QuotationLot[T](quoted, uid)

// TODO Does this need to be covariant? It is in current quill. Need to look up what use cases they are for covariant schemas.
case class SchemaMeta[T](val entity: Quoted[io.getquill.EntityQuery[T]], override val uid: String) extends QuotationLot[EntityQuery[T]](entity, uid)

case class QueryMeta[T, R](val entity: Quoted[Query[T] => Query[R]], override val uid: String, extract: R => T) extends QuotationLot[Query[T] => Query[R]](entity, uid)
