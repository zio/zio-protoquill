package miniquill.parser

import scala.quoted._


object MatchMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }

    def printMacImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
      class Operations(implicit val qctx: QuoteContext) extends TastyMatchers {
        import qctx.tasty._
        val any = anyRaw.unseal.underlyingArgument.seal
        
        object TupleName {
          def unapply(str: String): Boolean = str.matches("Tuple[0-9]+")
        }
        object TupleIdent {
          def unapply(term: Term): Boolean =
            term match {
              case Ident(TupleName()) => true
              case _ => false
            }
        }

        Untype(any.unseal) match {
          case Apply(TypeApply(Select(TupleIdent(), "apply"), types), values) =>
            println(s"============= Matched! ${values} ${types} =============")
          case other =>
            println(s"=============== Not Matched! =============")
            println(other.showExtractors)

            println("================= Pretty Tree =================")
            println(pprint.apply(other))
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

      println("================= Tree =================")
      println(any.show)

      println("================= Matchers =================")
      println(any.unseal.showExtractors)

      println("================= Pretty Tree =================")
      println(pprint.apply(any.unseal))

      '{ () }
    }
}
