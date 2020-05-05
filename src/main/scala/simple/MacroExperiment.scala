package simple

import scala.quoted._
import scala.quoted.matching.{Const => ConstExpr, _} // Including ExprSeq?
import miniquill.quoter.Dsl
import miniquill.parser.TastyMatchersContext

object MacroExperiment {
  

  inline def printTree(tree: Any): Any = ${ printTreeImpl('tree) }
  def printTreeImpl(tree: Expr[Any])(given qctx: QuoteContext): Expr[Any] = {
    import qctx.tasty.{given _, _}
    val tctx = new TastyMatchersContext
    import tctx._

    val resealed = tree.unseal.underlyingArgument.seal

    printer.lnf(resealed.unseal)

    resealed.unseal match {
      case Apply(Select(RawLambdaN(idents, body), "apply"), args) =>
        println("Matched!")
        println(idents)
        printer.lnf(body)
      // case Apply(Select(inside, "apply"), args) =>
      //   println("Matched Apply(Select())!")
      //   printer.lnf(inside)
      //   println(args.zipWithIndex.map((r, i) => s"(${i}): " + printer.str(r)).mkString)
      case other => 
        println("Not Matched")
    }

    //println(tree.unseal.underlyingArgument.seal.show)
    //printer.lnf(tree.unseal.underlyingArgument)
    tree
  }

}
