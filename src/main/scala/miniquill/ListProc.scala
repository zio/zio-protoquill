package miniquill

import scala.quoted._

object ListProc {
  inline def index[T](inline list: List[T], index:Int): T = ${ indexImpl('list, 'index) }
  def indexImpl[T: Type](list: Expr[List[T]], index: Expr[Int])(implicit qctx: QuoteContext): Expr[T] = {
    val tm = new miniquill.parser.TastyMatchersContext
    import tm._
    val indexValue = index match { case  Const(i: Int) => i }
    val exprs = UntypeExpr(list.unseal.underlyingArgument.seal) match { 
      case '{ scala.List.apply[T](${Varargs(args)}: _*) } => args  
      case _ => report.throwError("Does not match: " + list.unseal.showExtractors)
    }
    exprs(indexValue)
  }

  inline def tail[T](inline list: List[T]): List[T] = ${ tailImpl('list) }
  def tailImpl[T: Type](list: Expr[List[T]])(implicit qctx: QuoteContext): Expr[List[T]] = {
    val tm = new miniquill.parser.TastyMatchersContext
    import tm._
    val exprs = UntypeExpr(list.unseal.underlyingArgument.seal) match { 
      case '{ scala.List.apply[T](${Varargs(args)}: _*) } => args  
    }
    Expr.ofList(exprs.drop(1).toList)
  }

  transparent inline def isNil[T](inline list: List[T]): Boolean = ${ isNillImpl('list) }
  def isNillImpl[T: Type](list: Expr[List[T]])(implicit qctx: QuoteContext): Expr[Boolean] = {
    import qctx.reflect._
    val output =
      list match { 
        case '{ scala.List.apply[T](${Varargs(args)}: _*) } if (args.length == 0) => true
        case '{ scala.Nil } => true
        case _ => false
      }
    '{ ${Literal(Constant.Boolean(output)).seal.asExprOf[Boolean]} }
    //Expr(output)
  }

  transparent inline def isTrue: Boolean = ${ isTrueImpl }
  def isTrueImpl(using qctx: QuoteContext) = {
    import qctx.reflect._
    Literal(Constant.Boolean(true)).seal.asExprOf[Boolean]
  }


  transparent inline def length[T](inline list: List[T]): Int = ${ lengthImpl('list) }
  def lengthImpl[T: Type](list: Expr[List[T]])(implicit qctx: QuoteContext): Expr[Int] = {
    import qctx.reflect._
    val output =
      list match { 
        case '{ scala.List.apply[T](${Varargs(args)}: _*) } => args.length
        case '{ scala.Nil } => 0
      }
    //'{ ${Literal(Constant(output)).seal.asExprOf[Int]} }
    Expr(output)
  }
}
