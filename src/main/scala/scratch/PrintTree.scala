package scratch

import scala.quoted._

object printTree {
  inline def apply[T](inline tree: T): T = ${ printTreeImpl[T]('tree) }
  def printTreeImpl[T:Type](tree: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect._
    println(tree.show)
    printer.lnf(tree.asTerm)
    printer.lnf(tree.asTerm.underlyingArgument)
    tree
  }
}