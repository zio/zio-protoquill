package simple

import scala.quoted._

object MacroExperiment {
  case class Foo(value: String)

  inline def stuff: List[Any] = List.apply[Any](Foo("blah"), 3, 4.5, Foo("blah blah"))
  def reg_stuff: List[Any] = List.apply[Any](Foo("blah"), 3, 4.5, Foo("blah blah"))                                      

  inline def printTree(tree: Any): Any = ${ printTreeImpl('tree) }
  def printTreeImpl(tree: Expr[Any])(given qctx: QuoteContext): Expr[Any] = {
    import qctx.tasty.{given, _}
    printer.ln(tree.underlyingArgument.unseal)
    tree
  }

}


