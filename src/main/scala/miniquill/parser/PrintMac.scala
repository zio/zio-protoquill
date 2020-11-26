package miniquill.parser

import scala.quoted._
import io.getquill.ast.{Ident => Idnt}

object MatchMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }

    def printMacImpl(anyRaw: Expr[Any])(using Quotes): Expr[Unit] = {
      class Operations(implicit val qctx: Quotes) extends TastyMatchers {
        import quotes.reflect.{Ident => TIdent, Constant => TConstant, _}
        val any = Term.of(anyRaw).underlyingArgument.asExpr

        UntypeExpr(any) match {
          case Unseal(Block(parts, lastPart)) =>
            parts(0)
            
            parts.map(p => println("==== Matched! Exprs: " + p.showExtractors + " ===="))

            println(s"============= Matched! Expr: ${lastPart.showExtractors} =============")
          case other =>
            println(s"=============== Not Matched! =============")
            println(Term.of(other).showExtractors)

            println("================= Pretty Tree =================")
            println(pprint.apply(Term.of(other)))
        }
      }
          

      new Operations
      '{ () }
    }
}


object PrintMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
    def printMacImpl(anyRaw: Expr[Any])(using Quotes): Expr[Unit] = {
      import quotes.reflect._
      val any = Term.of(anyRaw).underlyingArgument.asExpr
      class Operations(implicit val qctx: Quotes) extends TastyMatchers {
        import quotes.reflect._
        println("================= Tree =================")
        println(any.show)

        println("================= Matchers =================")
        println(Untype(Term.of(any)).showExtractors)

        println("================= Pretty Tree =================")
        println(pprint.apply(Untype(Term.of(any))))
      }

      new Operations()
      '{ () }
    }
}
