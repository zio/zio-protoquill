package simple

import scala.quoted._
import scala.quoted.{Const => ConstExpr, _}
import miniquill.quoter.Dsl
import miniquill.parser.TastyMatchersContext

object MacroExperiment {
  

  inline def printTree(tree: Any): Any = ${ printTreeImpl('tree) }
  def printTreeImpl(tree: Expr[Any])(using Quotes): Expr[Any] = {
    import quotes.reflect._
    val tctx = new TastyMatchersContext
    import tctx._

    val resealed = Term.of(tree).underlyingArgument.asExpr

    printer.lnf(Term.of(resealed))

    Term.of(resealed) match {
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

    //println(Term.of(tree).underlyingArgument.asExpr.show)
    //printer.lnf(Term.of(tree).underlyingArgument)
    tree
  }

}
