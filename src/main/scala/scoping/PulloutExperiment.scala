package scoping

import scala.quoted._
import scala.deriving.ArrayProduct
//import dotty.tools.dotc.ast.untpd._

object PulloutExperiment {

  inline def printTree[T](value: T):T = ${ printTreeImpl('value) }
  def printTreeImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    //printer.ln(value.underlyingArgument.unseal)
    printer.ln("===================== printTree ================\n")
    printer.ln(value.underlyingArgument.unseal)
    value
  }
  
  def lookInside(value: Any): Any = value

  inline def parseTuple(input: Tuple): Tuple = ${parseTupleImpl('input)}
  def parseTupleImpl(input: Expr[Tuple])(given qctx: QuoteContext): Expr[Tuple] = {
    import qctx.tasty.{given, _}
    import scala.collection.mutable.ArrayBuffer

    object AsTerm {
      def unapply(tree: Tree) =
        if (tree.isInstanceOf[Term]) Some(tree.asInstanceOf[Term])
        else None
    }

    object Seal {
      def unapply[T](e: Term) = {
        implicit val ttpe: quoted.Type[T] = e.tpe.seal.asInstanceOf[quoted.Type[T]]
        Some(e.seal.cast[T])
      }
    }

    // TODO Make this tail recursive
    val accum = new TreeAccumulator[ArrayBuffer[Term]] {
      def foldTree(terms: ArrayBuffer[Term], tree: Tree)(given ctx: qctx.tasty.Context) = {
        if (tree.isInstanceOf[Term]) println(s"+++++++++++ It's a term: ${tree.asInstanceOf[Term]}")
        else println(s"+++++++++++ Tree is not a term: ${tree}")
        foldOverTree(terms, tree)
      }
    }
    accum.foldTree(new ArrayBuffer(), input.underlyingArgument.unseal)


    def recurseTuple(tup: Expr[Tuple]): List[Expr[Any]] =
      input.underlyingArgument match {
        case '{ () } => 
          printer.ln("=========== Matched :: Nil")
          Nil
        case '{ ($head *: ()) } => 
          printer.ln(s"=========== Matched: ${head.show} :: Nil")
          head :: Nil
        case '{ ($head *: (${tail}: Tuple)) } => 
          printer.ln(s"=========== Matched: ${head.show} :: ${tail.show}")
          head :: recurseTuple(tail)
        case '{ $other } =>
          printer.ln(s"=========== Other Tuple: ${other.show}")
          List()
      }

    println("Input:\n" + input.show)
    val instances = recurseTuple(input)

    //printer.ln(instances.map(_.underlyingArgument.show))

    val ret =
      instances.foldRight('{ (): Tuple })((elem, term) => '{ ( ${elem} *: ${term} ) })

    ret
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
        case t @ Apply(Ident("lookInside"), List(arg)) => 
          printer.ln("Found: " + t)
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