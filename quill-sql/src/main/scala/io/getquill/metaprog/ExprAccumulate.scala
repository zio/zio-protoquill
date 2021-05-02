package io.getquill.metaprog

import scala.quoted._

import scala.collection.mutable.ArrayBuffer

object ExprAccumulate {
  def apply[T: Type, ExpectedType](input: Expr[Any])(matcher: PartialFunction[Expr[Any], T])(using Quotes): List[T] = {
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
          // If it is a Quat we immediately know it's not a Uprootable (i.e. we have gone too far down the chain)
          expr match
            case _ if isQuat(expr) => expr
            // Not sure why but transformChildren causes a varargs case to fail with
            // "Expr cast exception Seq[TF] could not be cast to type _*"
            case Varargs(args) =>
              // Go through the children but don't transform the parent expression. This is okay
              // since we are a collecting over a mutable list anyway
              args.foreach(arg => transformChildren(arg))
              // just return the expr, we don't need to dive into it
              expr
            case _ => 
              super.transformChildren(expr)

        } catch {
          case e if e.getMessage.startsWith("Expr cast exception:") => //hello
            // println(
            //   s"============== Could not transform over expression:\n" +
            //   s"${io.getquill.util.Format.Expr(expr)}\n" +
            //   s"${e.getMessage}\n" +
            //   s"\n===========")
            expr
        }
      }

      def isQuat(expr: Expr[_]) =
        expr.asTerm.tpe <:< TypeRepr.of[io.getquill.quat.Quat]

      def transform[TF](expr: Expr[TF])(using Type[TF])(using Quotes): Expr[TF] = {
        if (!isQuat(expr))
          matcher.lift(expr) match
            case Some(result) =>
              buff += result
            case None =>

        expr.asTerm match
          // Not including this causes execption "scala.tasty.reflect.ExprCastError: Expr: [ : Nothing]" in certain situations
          case Repeated(Nil, Inferred()) => expr 
          case _ if (isQuat(expr)) => expr
          case _ => transformChildren[TF](expr)
      }
    }

    accum.transform(input)
    buff.toList
  }
}
