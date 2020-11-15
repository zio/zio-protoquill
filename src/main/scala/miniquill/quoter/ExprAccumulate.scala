package miniquill.quoter

import scala.quoted._

import scala.collection.mutable.ArrayBuffer
import scala.quoted.util.ExprMap

object ExprAccumulate {
  def apply[T](input: Expr[Any])(matcher: PartialFunction[Expr[Any], T])(using qctx: QuoteContext): List[T] = {
    import qctx.reflect.{Type => QType, _}

    val buff: ArrayBuffer[T] = new ArrayBuffer[T]()
    val accum = new ExprMap {

      // =================== Transforming Children ================
      //   Literal(("name"))
      //   ============== Could not transform over expression ===========
      //   scala.tasty.reflect.ExprCastError: Expr: ["name" : String]
      //   did not conform to type: String*
      override def transformChildren[TF](expr: Expr[TF])(using qctx: QuoteContext, tpe: Type[TF]): Expr[TF] = {
        try {
          super.transformChildren(expr)
        } catch {
          case e: scala.tasty.reflect.ExprCastError => //hello
            //println("============== Could not transform over expression ===========")
            //e.printStackTrace
            //printer.lnf(expr.unseal)
            expr
        }
      }

      def transform[TF](expr: Expr[TF])(using qctx: QuoteContext, tpe: Type[TF]): Expr[TF] = {
        matcher.lift(expr) match {
          case Some(result) => 
            buff += result
            expr
          case None =>
            expr
        }

        expr.unseal match {
          // Not including this causes execption "scala.tasty.reflect.ExprCastError: Expr: [ : Nothing]" in certain situations
          case Repeated(Nil, Inferred()) => expr 
          case _ => transformChildren[TF](expr)
        }
      }
    }

    accum.transform(input)
    buff.toList
  }
}
