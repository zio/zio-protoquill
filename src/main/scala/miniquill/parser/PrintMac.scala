package miniquill.parser

import scala.quoted._

object MatchLambdaMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }

    def printMacImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
      class Operations(implicit val qctx: QuoteContext) extends TastyMatchers {

        import qctx.tasty._
        val any = anyRaw.unseal.underlyingArgument.seal

        UntypeExpr(any) match {
          case Lambda1(arg, body) =>
            println("***************** MATCHED LAMBDA ONE ***************")
            println("Arg: " + arg)
            println("Body: " + body.unseal.showExtractors)

        // Untype(any.unseal) match {
        //   //case Lambda(arg, body) =>
        //   case Lambda(List(ValDef(arg,_,_)), body) =>
        //   //case Lambda(List(ValDef(arg, Inferred(), any1)), body) =>
        //     println("***************** MATCHED LAMBDA ONE ***************")
        //     println("Arg: " + arg)
        //     println("Body: " + body.showExtractors)

          /*
          Lambda(
            List(ValDef(ident, Inferred(), None)), 
            methodBody
          )
          Lambda(
            List(ValDef(p,Ident(String),EmptyTree))), 
            Apply(Select(Ident("p"), "length"), Nil
          )
          */

          case _ => 
            println("***************** DID NOT MATCH LAMBDA ONE ***************")
        }

      }
      new Operations
      '{ () }
    }
}


object MatchMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }

    def printMacImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
      class Operations(implicit val qctx: QuoteContext) extends TastyMatchers {

        import qctx.tasty._
        val any = anyRaw.unseal.underlyingArgument.seal
        Untype(any.unseal) match {
          case Apply(Select(query, "insert"), assignments) =>
            println("============= yayaya WE MATCHED ==============")
            val assignment = assignments(0)
            println("================== First Term: =======================\n" + 
              assignment.showExtractors
            )

            println("=========== Trying to Match First Term =============")
            assignment match {
              case
                Seal(Lambda1(lamvar, content)) => //helloooo
                println("^^^^^^^^^^^^^^^^^^^^ LAMBDA MATCHES ^^^^^^^^^^^^^^^^^")
                println(content.unseal.showExtractors)

                content.unseal match {
                  case
                    Apply(TypeApply(
                      Select(Apply(
                        TypeApply(Ident("ArrowAssoc"), List(Inferred())), 
                        List(Select(Ident("p"), "name"))
                      ), "->"), 
                      List(Inferred())
                    ), List(Literal(Constant("Joe")))) => println("******* INNER MATCH *****")
                  case _ => println("***** NO INNER MATCH *****")
                }

              case 
                Block(
                  List(
                    DefDef(
                      anon, Nil, 
                      List(List(ValDef("p", Inferred(), None))),
                      Inferred(), 
                      Some(
                        Apply(TypeApply(
                          Select(Apply(
                            TypeApply(Ident("ArrowAssoc"), List(Inferred())), 
                            List(Select(Ident("p"), "name"))
                          ), "->"), 
                          List(Inferred())
                        ), 
                        List(Literal(Constant("Joe"))))
                      )
                    )
                  ), Closure(Ident(anon1), None)
                )
                => println("!!!!!!!!!!!!!!!!!!!! HUZZAH WE MATCHED INNER CLAUSE !!!!!!!!!!!!!!!!!!!!")
              case _ =>
                println("&&&&&&&&&&&&&& NOPE, TRY AGAIN &&&&&&&&&&&&&&")
            }

          case _ =>
            println("Nope, we did not match")
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
