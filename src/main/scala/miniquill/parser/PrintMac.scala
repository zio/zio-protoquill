package miniquill.parser

import scala.quoted._

object Mac {
  inline def enter(inline str: String): Unit = ${ enterImpl('str) }
  def enterImpl(str: Expr[String])(using qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._
    println(pprint(str.unseal.underlyingArgument))
    '{ () }
  }

  inline def passThrough(inline str: String): String = ${ passThroughImpl('str) }
  def passThroughImpl(str: Expr[String])(using qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    class Operations(implicit val qctx: QuoteContext) extends TastyMatchers {
      //printExpr(str)
    }
    new Operations()
    str
  }

  
}

object MatchMac {
    import miniquill.quoter.QueryDsl._
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
    def printMacImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
      anyRaw match {
        case '{ ($str: String).like($other) } => println("Matched!")
        case _ => println("Not Matched!")
      }
      '{ () }
    }
}

object PrintMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
    def printMacImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
      import qctx.tasty._
      val any = anyRaw //.unseal.underlyingArgument.seal
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
