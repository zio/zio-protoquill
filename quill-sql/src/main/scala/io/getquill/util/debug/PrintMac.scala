package io.getquill.util.debug

import io.getquill.metaprog.Extractors._
import io.getquill.util.Format

import scala.quoted._

object PrintMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
    def printMacImpl(anyRaw: Expr[Any])(using Quotes): Expr[Unit] = {
      import quotes.reflect._
      val any = anyRaw.asTerm.underlyingArgument.asExpr
      class Operations(implicit val qctx: Quotes) {
        import quotes.reflect._
        println("================= Tree =================")
        println(Format(Printer.TreeShortCode.show(any.asTerm)))

        println("================= Matchers =================")
        println(Format(Printer.TreeStructure.show(Untype(any.asTerm))))

        //println("================= Pretty Tree =================")
        //println(pprint.apply(Untype(any.asTerm)))
      }

      new Operations()
      '{ () }
    }
}
