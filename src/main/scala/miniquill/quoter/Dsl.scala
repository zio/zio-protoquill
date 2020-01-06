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

case class Quoted[+T](val ast: Ast, lifts: Tuple) {  //, liftings: scala.collection.Map[String, Any]
  override def toString = ast.toString
  def unquote: T = ???
}

// TODO Rename to QuotedScalarValue
// Scalar value Vase holds a scala asclar value's tree until it's parsed
case class ScalarValueVase[T](value: T, uid: String)



object QuoteDsl {



  // TODO Move to a LiftDsl trait
  //inline def extractLifts(input: Any): Tuple = ${ extractLiftsImpl('input) }
  def extractLifts(input: Expr[Any])(given qctx: QuoteContext): Expr[Tuple] = {
    import qctx.tasty.{_, given}
    import scala.collection.mutable.ArrayBuffer

    val accum = new TreeAccumulator[ArrayBuffer[Term]] {
      def foldTree(terms: ArrayBuffer[Term], tree: Tree)(implicit ctx: Context) = tree match {
        case vase @ Apply(TypeApply(Select(Ident("ScalarValueVase"), "apply"), _), List(scalaTree, Literal(Constant(uid)))) =>
          // printer.ln("Found: " + vase)
          terms += vase
        case _ =>
          printer.ln("***** NOT FOUND ****")
          foldOverTree(terms, tree)
      }
    }
    val vases = accum.foldTree(ArrayBuffer.empty, input.underlyingArgument.unseal)

    val vasesTuple = vases.foldRight('{ (): Tuple })((elem, term) => '{ (${elem.seal} *: ${term}) })

    //printer.ln("=========== Found Vases =========\n" + vasesTuple.underlyingArgument.unseal.show)

    vasesTuple
  }

  // or maybe implemet this with whitebox macros and a scalar value lift instead of with implicit conversions
  inline def lift[T](value: T): T = ${ liftImpl('value) }
  def liftImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    val uuid = java.util.UUID.randomUUID().toString
    // Return the value of a created ScalarValueVase. The ScalarValueVase will be in the Scala AST
    // (i.e. and it will be parsed correctly)
    // with the generated UUID but the typing is correct since 'value' is returned.
    '{ ScalarValueVase[T]($value, ${Expr(uuid)}).value }
  }


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


  inline implicit def unquote[T](quoted: =>Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T: Type](quoted: Expr[Quoted[T]])(given qctx: QuoteContext): Expr[T] = {
    '{
      ${quoted}.unquote
    }
  }

  def querySchema[T](entity: String): EntityQuery[T] = ???

}
