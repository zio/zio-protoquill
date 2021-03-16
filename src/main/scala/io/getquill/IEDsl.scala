package io.getquill

import io.getquill.quat.Quat
import io.getquill.quat.quatOf
import io.getquill.quat.productQuatOf
import io.getquill.ast._
import io.getquill.{ lib => qlib }
import io.getquill.Quoted
import io.getquill.context.UnquoteMacro

object IEDsl {
  import Macro._

  case class Query[T](ast: Ast)

  inline def query[T] = Query[T](Entity(typeName[T], List(), productQuatOf[T])) // need to transform quat into product before lifting
  extension [T](inline q: Query[T])
    inline def filter(inline f: T => Boolean) =
      //Query[T](Filter(q.ast, Ident("id", Quat.Value), parse(f)))
      // val param = Ident(lambda1Param(f), quatOf[T]) // can't make variables like this & splice them because quill that doesn't work in ast splicing?
      // technically the Quill parser should understand it. Need to look into it
      Query[T](Filter(parse(qlib.unquote(toQuote(q))), Ident(lambda1Param(f), quatOf[T]), FunctionApply(parse(f), List(Ident(lambda1Param(f), quatOf[T])))))

  inline def toQuote[T](inline qry: Query[T]): Quoted[io.getquill.Query[T]] = ${ Macro.toQuoteImpl[T]('qry) }
  
    

  object Macro {
    import scala.quoted._
    import io.getquill.metaprog.Extractors
    import io.getquill.parser._
    import io.getquill.parser.Parser.Implicits._

    def toQuoteImpl[T: Type](qry: Expr[Query[T]])(using qctx: Quotes): Expr[Quoted[io.getquill.Query[T]]] = {
      class Ext(implicit override val qctx: Quotes) extends Extractors {
        import qctx.reflect._
        def extractAst(expr: Expr[_]): Expr[Ast] =  
          val underlying = expr.asTerm.underlyingArgument.asExpr
          //println(Printer.TreeShortCode.show(expr.asTerm.underlyingArgument))
          // can't unlift AST at this point since it might be spliced with variables?
          UntypeExpr(underlying) match {
            case '{ Query.apply[t]($ast) } => ast
            case _ => 
              //println(Printer.TreeStructure.show(expr.asTerm.underlyingArgument))
              report.throwError("Could not extract from Query object")
          }
      }
      val ast = new Ext().extractAst(qry)
      '{ Quoted($ast, Nil, Nil) }
    }

    inline def spliceAst(code: Ast): Ast = ${ spliceAstImpl('code) }
    def spliceAstImpl(code: Expr[Ast])(using qctx: Quotes): Expr[Ast] = 
      import qctx.reflect._
      println("==================== Unlifting ====================")
      val unliftedAst = Unlifter(code.asTerm.underlyingArgument.asExprOf[Ast])
      println(s"==================== Splicing: ${io.getquill.util.Messages.qprint(unliftedAst)} ====================")
      Lifter(unliftedAst)

    inline def parse[T](code: T): Ast = ${ parseImpl[T]('code) }
    def parseImpl[T: Type](code: Expr[T])(using qctx: Quotes): Expr[Ast] =
      import qctx.reflect._
      //println("==== Valid: " + Printer.TreeStructure.show(underlying.asTerm))
      val ast = ParserLibrary.apply.seal.apply(code.asTerm.underlyingArgument.asExpr)
      Lifter(ast)

    inline def typeName[T]: String = ${ typeNameImpl[T] }
    def typeNameImpl[T: Type](using qctx: Quotes): Expr[String] =
      import qctx.reflect._
      Expr(TypeRepr.of[T].classSymbol.get.name)

    inline def lambda1Param[T, R](inline f: T => Any): String = ${ lambda1ParamImpl('f) }
    def lambda1ParamImpl[T: Type, R: Type](f: Expr[T => Any])(using qctx: Quotes): Expr[String] = {
      import qctx.reflect._

      class Ext(implicit override val qctx: Quotes) extends Extractors {
        def apply(expr: Expr[_]) =  
          val underlying = expr.asTerm.underlyingArgument.asExpr
          underlying match
            case Lambda1(param, _, _) =>  Expr(param)
            case _ => report.throwError("Invalid")
      }

      new Ext().apply(f)
    }
  }
}



