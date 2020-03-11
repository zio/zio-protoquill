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

class Query[+T] {
  def map[R](f: T => R): Query[R] = new Query[R]
  def foo(): String = "hello"
}

class EntityQuery[T] extends Query[T] // TODO can have a list of column renames?

// TODO lifts needs to be List of Planter to allow QueryLifts
case class Quoted[+T](val ast: Ast, lifts: List[ScalarPlanter[_, _]], runtimeQuotes: List[QuotationVase])
  //override def toString = ast.toString
  // make a function that uses a stateless transformer to walk through the tuple,
  // gather the lifted quoted blocks, splice their qutations into the ast, and then
  // add their lifted values into the parent tuple.... basically a runtime
  // flattening of the tree. This is the mechanism that will be used by the 'run' function
  // for dynamic queries


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

// Quotations go from a QuotationBin directly inline into the tree or put into a QuotationVase
// to be added into the runtime inlining later
case class QuotationBin[+T](quoted: Quoted[T], uid: String) {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}
