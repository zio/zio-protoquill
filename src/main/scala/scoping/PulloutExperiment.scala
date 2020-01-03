package scoping

import scala.quoted._
import scala.quoted.matching._
import scala.deriving.ArrayProduct
//import dotty.tools.dotc.ast.untpd._

case class LookInside(value: Any, id: String)

object PulloutExperiment {

  inline def printTree[T](value: T):T = ${ printTreeImpl('value) }
  def printTreeImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    //printer.ln(value.underlyingArgument.unseal)
    printer.ln("===================== printTree ================\n")
    printer.ln(value.underlyingArgument.unseal)
    println(value.underlyingArgument.unseal.showExtractors)
    value
  }
  
  
  inline def lookInside(value: Any): LookInside = ${lookInsideImpl('value)}
  def lookInsideImpl(value: Expr[Any])(given qctx: QuoteContext): Expr[LookInside] = {
    val uuid = java.util.UUID.randomUUID().toString
    '{ LookInside($value, ${Expr(uuid)}) }
  }

  // TODO nest step would be to extract all uuids and get a particular one?

  // Then next step afterward would be to have a function that adds a value to a tuple

  inline def parseTuple(input: Tuple): List[LookInside] = ${parseTupleImpl('input)}
  def parseTupleImpl(input: Expr[Tuple])(given qctx: QuoteContext): Expr[List[LookInside]] = {
    import qctx.tasty.{given, _}
    import scala.collection.mutable.ArrayBuffer

    // Can also expore using TreeAccumulator to find LookInside instances
    // this might be easier however
  
    '{ $input.asInstanceOf[Product].productIterator.map(_.asInstanceOf[LookInside]).toList }
  }

  inline def pullout(input: Any): Tuple = ${pulloutImpl('input)}
  def pulloutImpl(input: Expr[Any])(given qctx: QuoteContext): Expr[Tuple] = {
    import qctx.tasty._
    import qctx.tasty.given
    //import qctx.tasty.given_IsInstanceOf_Term
    import scala.collection.mutable.ArrayBuffer

    printer.ln(input.underlyingArgument.unseal)

    
    val accum = new TreeAccumulator[ArrayBuffer[Term]] {
      def foldTree(terms: ArrayBuffer[Term], tree: Tree)(implicit ctx: Context) = tree match {
        case arg @ Apply(Select(Ident("LookInside"), "apply"), List(transportValue, Literal(Constant(uid)))) => 
          printer.ln("Found: " + arg)
          terms += arg
        case _ => 
          printer.ln("***** NOT FOUND ****")
          foldOverTree(terms, tree)
      }
    }

    //printer.ln(input.underlyingArgument.unseal)
    //printer.ln(input.underlyingArgument.unseal.showExtractors)

    val instances = accum.foldTree(ArrayBuffer.empty, input.underlyingArgument.unseal)

    instances.zipWithIndex.map { case (v, i) => printer.ln(s"Element: ($i) $v") }

    val ret =
     instances.foldRight('{ (): Tuple })((elem, term) => '{ ( ${elem.seal} *: ${term} ) })

    printer.ln("=========== Pullout Value =========\n" + ret.underlyingArgument.unseal.show)

    ret
  }
}