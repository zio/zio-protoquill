package miniquill.quoter

import miniquill.ast._
import miniquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import derivation._
import scala.deriving._

class Query[+T] {
  def map[R](f: T => R): Query[R] = new Query[R]
  def foo(): String = "hello"
}

class EntityQuery[T] extends Query[T] // TODO can have a list of column renames?

case class Quoted[+T](val ast: Ast) {  //, liftings: scala.collection.Map[String, Any]
  override def toString = ast.toString
  def unquote: T = ???
}



object QuoteDsl {

  def parserFactory: (QuoteContext) => PartialFunction[Expr[_], Ast] = 
    (qctx: QuoteContext) => new Parser(given qctx)

  def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: QuoteContext) => new Lifter(given qctx)

  inline def query[T]: EntityQuery[T] = new EntityQuery

  inline def quote[T](body: =>T): Quoted[T] = ${ quoteImpl[T]('body) }

  def quoteImpl[T: Type](body: Expr[T])(given qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given}

    // TODo add an error if body cannot be parsed
    val ast = parserFactory(qctx).apply(body)

    println(ast)

    // TODO Add an error if the lifting cannot be found
    val reifiedAst = lifterFactory(qctx)(ast)

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
