package scratch

import scala.quoted._

object printTree {
  inline def apply[T](inline tree: T): T = ${ printTreeImpl[T]('tree) }
  def printTreeImpl[T:Type](tree: Expr[T])(using qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given _, _}
    println(tree.show)
    printer.lnf(tree.unseal)
    printer.lnf(tree.unseal.underlyingArgument)
    tree
  }
}