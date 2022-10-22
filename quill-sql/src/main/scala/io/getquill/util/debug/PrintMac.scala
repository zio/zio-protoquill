package io.getquill.util.debug

import io.getquill.metaprog.Extractors._
import io.getquill.util.Format

import scala.quoted._
import io.getquill.metaprog.DeserializeAstInstances

object PrintMac {

  inline def apply(inline any: Any, inline showDetail: Boolean = false, inline deserializeAst: Boolean = false): Unit = ${ printMacImpl('any, 'showDetail, 'deserializeAst) }
  inline def passthrough(inline any: Any, inline showDetail: Boolean = false, inline deserializeAst: Boolean = false): Any = ${ printMacImpl('any, 'showDetail, 'deserializeAst) }

  def printMacImpl(anyRaw: Expr[Any], showDetailRaw: Expr[Boolean], deserializeAstRaw: Expr[Boolean])(using Quotes): Expr[Any] = {
    import quotes.reflect._
    val showDetail = Expr.unapply(deserializeAstRaw).getOrElse { report.throwError("showDetail must be a constant value true/false") }
    val deserializeAst = Expr.unapply(deserializeAstRaw).getOrElse { report.throwError("deserializeAst must be a constant value true/false") }

    val any = anyRaw.asTerm.underlyingArgument.asExpr
    val deser =
      if (deserializeAst)
        DeserializeAstInstances(any)
      else
        any

    println("================= Tree =================")
    println(Format(Printer.TreeShortCode.show(deser.asTerm)))

    if (showDetail) {
      println("================= Detail =================")
      println(Format(Printer.TreeStructure.show(Untype(deser.asTerm))))
    }

    // println("================= Pretty Tree =================")
    // println(pprint.apply(Untype(any.asTerm)))

    any
  }
}
