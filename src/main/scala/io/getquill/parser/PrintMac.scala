package io.getquill.parser

import scala.quoted._

object Mac {
  inline def enter(inline str: String): Unit = ${ enterImpl('str) }
  def enterImpl(str: Expr[String])(using qctx: Quotes): Expr[Unit] = {
    import qctx.reflect._
    println(pprint(str.asTerm.underlyingArgument))
    '{ () }
  }

  inline def passThrough(inline str: String): String = ${ passThroughImpl('str) }
  def passThroughImpl(str: Expr[String])(using qctx: Quotes): Expr[String] = {
    import qctx.reflect._
    class Operations(implicit val qctx: Quotes) extends TastyMatchers {
      //printExpr(str)
    }
    new Operations()
    str
  }

  
}

object MatchMac {
    import io.getquill.QueryDsl._
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }

    def printMacImpl(anyRaw: Expr[Any])(using Quotes): Expr[Unit] = {
      class Operations(implicit val qctx: Quotes) extends TastyMatchers {
        import quotes.reflect.{Ident => TIdent, Constant => TConstant, _}
        val any = anyRaw.asTerm.underlyingArgument.asExpr

        any match {
          case CaseClassCreation(ccName, fields, args) =>
            if (fields.length != args.length) 
              throw new IllegalArgumentException(s"In Case Class ${ccName}, does not have the same number of fields (${fields.length}) as it does arguments ${args.length} (fields: ${fields}, args: ${args.map(_.show)})")
            println( fields.zip(args.map(_.show)) )
            

          // case ast if (ast.asTerm.tpe <:< TypeRepr.of[Product]) =>
          //   println("~~~~~~~~~~~~~~~~~~~ MATCHING ~~~~~~~~~~~~~~~")
          //   println(Printer.TreeStructure.show(ast.asTerm))
          //   val fields = ast.asTerm.tpe.classSymbol.get.caseFields.map(_.name)

          //   ast match {
          //     case CaseClassCreation(ccName, args) =>
          //       if (fields.length != args.length) 
          //         throw new IllegalArgumentException(s"In Case Class ${ccName}, does not have the same number of fields (${fields.length}) as it does arguments ${args.length} (fields: ${fields}, args: ${args.map(_.show)})")
          //       println( fields.zip(args.map(_.show)) )
          //     case _ => 
          //       println("~~~~~~~~~~ NOT MATCHED Matching Case Class Apply ~~~~~~~~~~~")
          //   }

          //   println( fields )

          case other =>
            println(s"=============== Not Matched! =============")
            println(Printer.TreeStructure.show(other.asTerm))

            println("================= Pretty Tree =================")
            println(pprint.apply(other.asTerm))
        }
      }
      new Operations()
      '{ () }
    }
}

object PrintMac {
    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }
    def printMacImpl(anyRaw: Expr[Any])(using Quotes): Expr[Unit] = {
      import quotes.reflect._
      val any = anyRaw.asTerm.underlyingArgument.asExpr
      class Operations(implicit val qctx: Quotes) extends TastyMatchers {
        import quotes.reflect._
        println("================= Tree =================")
        println(any.show)

        println("================= Matchers =================")
        println(Printer.TreeStructure.show(Untype(any.asTerm)))

        println("================= Pretty Tree =================")
        println(pprint.apply(Untype(any.asTerm)))
      }

      new Operations()
      '{ () }
    }
}
