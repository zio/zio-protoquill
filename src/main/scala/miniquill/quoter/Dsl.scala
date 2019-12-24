package miniquill.quoter

import miniquill.ast._
import miniquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import derivation._
import scala.deriving._
import Parser._

class Query[+T] {
  def map[R](f: T => R): Query[R] = new Query[R]
  def foo(): String = "hello"
}

class EntityQuery[T] extends Query[T] // TODO can have a list of column renames?

object QueryDsl {
  // throws an exception if Query/EntityQuery defined in here. Should investigate why

  inline def query[T]: EntityQuery[T] = new EntityQuery
}

// Liftings are a map of UUID->Value pairs, going to also need to store
// type tags since need to know types of things with generic params e.g. postgres arrays
case class Quoted[+T](val ast: Ast) {  //, liftings: scala.collection.Map[String, Any]
  override def toString = ast.toString
  // The unquote method is there to signal the parser not splice in the tree
  // however, we should also plug in the body of the tree into this value
  // so that you can use .unquote outside of macro code to get back othe original tree
  def unquote: T = ???
}

object QuoteDsl {
  import QueryDsl._

  inline def quote[T](body: =>T): Quoted[T] = ${ quoteImpl[T]('body) }

  def gatherLiftings(body: Expr[_]): Expr[scala.collection.Map[String, Any]] = {
    // use a tree-walker to
    // gather liftings from Unquote blocks (i.e. the ones the parser parses, both as result of 'unquote' and '.unquote' method invocation)
    // -- these will be map objects
    // gather liftings from Lift blocks
    // -- these will be individual key values pairs
    // combine the found maps with the key/value pairs returning an expression
    // that yields a map

    null // back here
  }

  def quoteImpl[T: Type](body: Expr[T])(given qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given}

    // TODO use a tree walkler to gather maps from quoted blocks
    // we could do this in the parser but then the parser would have to be
    // a stateful transformer. I don't want to require it to be that since
    // I want the parser to be stateless (or at least to not have to force)
    // the user into implementing a stateful transformer as the parser
    // val liftings: List[Map[String, Any]] = gatherLiftings(body)

    

    val ast = astParser(body)

    println(ast)

    val reifiedAst = lift(ast)

    '{       
      Quoted[T](${reifiedAst})
    }
  }


  def runQuery[T](query: Quoted[Query[T]]): String = ???

  def run[T](query: Quoted[T]): String = {
    query.ast.toString
  }

  import scala.language.implicitConversions


  inline implicit def unquote[T](quoted: =>Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T: Type](quoted: Expr[Quoted[T]])(given qctx: QuoteContext): Expr[T] = {
    '{
      ${quoted}.unquote
    }
  }

  def querySchema[T](entity: String): EntityQuery[T] = ???

}
