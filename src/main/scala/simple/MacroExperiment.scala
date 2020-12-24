package simple

import scala.quoted._
import scala.quoted.{Const => ConstExpr, _}
import io.getquill.quoter.Dsl
import io.getquill.parser.TastyMatchersContext

object MacroExperiment {
  

  inline def printTree(tree: Any): Any = ${ printTreeImpl('tree) }
  def printTreeImpl(tree: Expr[Any])(using Quotes): Expr[Any] = {
    import quotes.reflect._
    val tctx = new TastyMatchersContext
    import tctx._

    val resealed = tree.asTerm.underlyingArgument.asExpr

    printer.lnf(resealed.asTerm)

    resealed.asTerm match {
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

    //println(tree.asTerm.underlyingArgument.asExpr.show)
    //printer.lnf(tree.asTerm.underlyingArgument)
    tree
  }

}
