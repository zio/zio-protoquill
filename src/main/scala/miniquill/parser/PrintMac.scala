package miniquill.parser

import scala.quoted._
import io.getquill.ast.{Ident => Idnt}

object MatchMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }

    def printMacImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
      class Operations(implicit val qctx: QuoteContext) extends TastyMatchers {
        import qctx.tasty.{Type => TType, Ident => TIdent, Constant => TConstant, _}
        val any = anyRaw.unseal.underlyingArgument.seal

        UntypeExpr(any) match {
          case Unseal(Block(parts, lastPart)) =>
            parts(0)
            
            parts.map(p => println("==== Matched! Exprs: " + p.showExtractors + " ===="))

            println(s"============= Matched! Expr: ${lastPart.showExtractors} =============")
          case other =>
            println(s"=============== Not Matched! =============")
            println(other.unseal.showExtractors)

            println("================= Pretty Tree =================")
            println(pprint.apply(other.unseal))
        }
      }
          

      new Operations
      '{ () }
    }
}


object PrintMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
    def printMacImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
      import qctx.tasty._
      val any = anyRaw.unseal.underlyingArgument.seal
      class Operations(implicit val qctx: QuoteContext) extends TastyMatchers {
        println("================= Tree =================")
        println(any.show)

        println("================= Matchers =================")
        println(Untype(any.unseal).showExtractors)

        println("================= Pretty Tree =================")
        println(pprint.apply(Untype(any.unseal)))
      }

      new Operations()
      '{ () }
    }
}
