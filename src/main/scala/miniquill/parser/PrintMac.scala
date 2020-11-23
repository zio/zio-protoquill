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
          case Unseal(Match(expr, List(CaseDef(field, guard, body)))) =>
            guard match {
              case Some(guardTerm) =>
                report.throwError("Guards in case- match are not supported", guardTerm.seal)
              case None =>
            }

            // def propertyAt(path: List[String]) =
            //   path.foldLeft(tuple) {
            //     case (tup, elem) => Property(tup, elem)
            //   }

            def tupleBindsPath(field: Tree, path: List[String] = List()): List[(Idnt, List[String])] =
              UntypeTree(field) match {
                case Bind(name, TIdent(_)) => List(Idnt(name) -> path)
                case Unapply(Method0(TupleIdent(), "unapply"), something, binds) => 
                  binds.zipWithIndex.flatMap { case (bind, idx) =>
                    tupleBindsPath(bind, path :+ s"_${idx + 1}")
                  }
                case other => report.throwError(s"Invalid Pattern Matching Term: ${other.showExtractors}")
              }

            println(s"============= Matched! Expr: ${expr.showExtractors} Fields: ${tupleBindsPath(field)} Body: ${body.showExtractors} =============")
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
