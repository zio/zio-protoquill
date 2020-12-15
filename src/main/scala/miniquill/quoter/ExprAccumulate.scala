package miniquill.quoter

import scala.quoted._

import scala.collection.mutable.ArrayBuffer

object ExprAccumulate {
  def apply[T](input: Expr[Any])(matcher: PartialFunction[Expr[Any], T])(using Quotes): List[T] = {
    import quotes.reflect._

    val buff: ArrayBuffer[T] = new ArrayBuffer[T]()
    val accum = new ExprMap {

      // =================== Transforming Children ================
      //   Literal(("name"))
      //   ============== Could not transform over expression ===========
      //   scala.tasty.reflect.ExprCastError: Expr: ["name" : String]
      //   did not conform to type: String*
      override def transformChildren[TF](expr: Expr[TF])(using Type[TF])(using Quotes): Expr[TF] = {
        try {
          super.transformChildren(expr)
        } catch {
          case e if e.getMessage.startsWith("Expr cast exception:") => //hello
            //println("============== Could not transform over expression ===========")
            //e.printStackTrace
            //printer.lnf(Term.of(expr))
            expr
        }
      }

      def transform[TF](expr: Expr[TF])(using Type[TF])(using Quotes): Expr[TF] = {
        matcher.lift(expr) match {
          case Some(result) => 
            buff += result
            expr
          case None =>
            expr
        }

        Term.of(expr) match {
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
