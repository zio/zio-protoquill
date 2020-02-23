package miniquill.quoter

import io.getquill.ast.Ast
import miniquill.parser._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import derivation._
import scala.deriving._
import scala.quoted.matching.Const

class Query[+T] {
  def map[R](f: T => R): Query[R] = new Query[R]
  def foo(): String = "hello"
}

class EntityQuery[T] extends Query[T] // TODO can have a list of column renames?

case class Quoted[+T](val ast: Ast, lifts: Tuple) {
  //override def toString = ast.toString
  // make a function that uses a stateless transformer to walk through the tuple,
  // gather the lifted quoted blocks, splice their qutations into the ast, and then
  // add their lifted values into the parent tuple.... basically a runtime
  // flattening of the tree. This is the mechanism that will be used by the 'run' function
  // for dynamic queries
}

// TODO Rename to ScalarValueVase
// Scalar value Vase holds a scala asclar value's tree until it's parsed
case class ScalarValueVase[T](value: T, uid: String) {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}
case class QuotationVase[+T](quoted: Quoted[T], uid: String) {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}


object QuoteDsl {



  // TODO Move to a LiftDsl trait
  //inline def extractLifts(input: Any): Tuple = ${ extractLiftsImpl('input) }


  
  def extractLifts(input: Expr[Any])(given qctx: QuoteContext): Expr[Tuple] = {
    import qctx.tasty.{Type => TType, _, given _}
    import scala.collection.mutable.ArrayBuffer
    import scala.quoted.util.ExprMap

    // println("++++++++++++++++ Input Tree ++++++++++++++++")
    // printer.ln(input.unseal)
    // printer.ln(input.unseal.showExtractors)
    val quotationParser = new miniquill.parser.QuotationParser
    import quotationParser._

    val lifts = FindLifts[Any](input)
    val vasesTuple = 
      lifts
        .distinctBy((uid, value) => uid) // dedupe by since everything with the same uuid is the same thing
        .map(_._2)
        .foldRight('{ (): Tuple })((elem, term) => '{ (${elem} *: ${term}) })

    vasesTuple
  }

  // or maybe implemet this with whitebox macros and a scalar value lift instead of with implicit conversions
  inline def lift[T](inline value: T): T = ${ liftImpl('value) }
  def liftImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    val uuid = java.util.UUID.randomUUID().toString
    // Return the value of a created ScalarValueVase. The ScalarValueVase will be in the Scala AST
    // (i.e. and it will be parsed correctly)
    // with the generated UUID but the typing is correct since 'value' is returned.
    '{ ScalarValueVase[T]($value, ${Expr(uuid)}).unquote }
  }


  def parserFactory: (QuoteContext) => PartialFunction[Expr[_], Ast] = 
    (qctx: QuoteContext) => new Parser(given qctx)

  def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: QuoteContext) => new Lifter(given qctx)

  inline def query[T]: EntityQuery[T] = new EntityQuery

  inline def quote[T](inline bodyExpr: T): Quoted[T] = ${ quoteImpl[T]('bodyExpr) }

  def quoteImpl[T: Type](bodyRaw: Expr[T])(given qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given _}
    // NOTE Can disable if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = bodyRaw.unseal.underlyingArgument.seal

    // TODo add an error if body cannot be parsed
    val ast = parserFactory(qctx).apply(body)

    println("Ast Is: " + ast)

    // TODO Add an error if the lifting cannot be found
    val reifiedAst = lifterFactory(qctx)(ast)

    val lifts = extractLifts(body)

    '{       
      Quoted[T](${reifiedAst}, ${lifts})
    }
  }


  def runQuery[T](query: Quoted[Query[T]]): String = ???

  def run[T](query: Quoted[T]): String = {
    query.ast.toString
  }

  import scala.language.implicitConversions

  // TODO Or like this?
  //inline implicit def unquoteScalar[T](quoted: =>ScalarValueVase[T]): T = quoted.value
  // inline implicit def unquoteScalar[T](quoted: =>ScalarValueVase[T]): T = ${ unquoteScalarImpl('quoted) }
  // def unquoteScalarImpl[T: Type](quoted: Expr[ScalarValueVase[T]])(given qctx: QuoteContext): Expr[T] = {
  //   '{
  //     ${quoted}.value
  //   }
  // }

  // TODO Should also probably name a method for this so don't need to enable explicit conversion
  inline implicit def unquote[T](inline quoted: Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T: Type](quoted: Expr[Quoted[T]])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    '{
      QuotationVase[T](${quoted}, ${Expr(java.util.UUID.randomUUID().toString)}).unquote
    }
  }

  def querySchema[T](entity: String): EntityQuery[T] = ???

}
