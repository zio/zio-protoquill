package io.getquill.util.debug

import io.getquill.metaprog.Extractors._
import io.getquill.util.Format

import scala.quoted._
import io.getquill.metaprog.DeserializeAstInstances

object PrintMac {
    inline def apply(inline any: Any, inline deserializeAst: Boolean = false): Unit = ${ printMacImpl('any, 'deserializeAst) }
    def printMacImpl(anyRaw: Expr[Any], deserializeAstRaw: Expr[Boolean])(using Quotes): Expr[Unit] = {
      import quotes.reflect._

      val deserializeAst =
        deserializeAstRaw match
          case '{ true } => true
          case '{ false } => false
          case _ => report.throwError("deserializeAst must be a constant value true/false")

      val any = anyRaw.asTerm.underlyingArgument.asExpr
      val deser =
        if (deserializeAst)
          DeserializeAstInstances(any)
        else
          any

      println("================= Tree =================")
      println(Format(Printer.TreeShortCode.show(deser.asTerm)))

      println("================= Matchers =================")
      println(Format(Printer.TreeStructure.show(Untype(deser.asTerm))))

      //println("================= Pretty Tree =================")
      //println(pprint.apply(Untype(any.asTerm)))

      '{ () }
    }
}
