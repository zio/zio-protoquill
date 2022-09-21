package io.getquill.metaprog

import scala.quoted._

import scala.collection.mutable.ArrayBuffer
import io.getquill.util.Format
import scala.util.Try

/** Remove all instances of SerialHelper.fromSerialized from a tree (for printing purposes) */
object DeserializeAstInstances:
  def apply[T: Type](input: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.{Try => _, _}
    import io.getquill.parser.SerialHelper
    import io.getquill.parser.Lifter
    import io.getquill.metaprog.Extractors._

    def isQuat(expr: Expr[_]) =
      expr.asTerm.tpe <:< TypeRepr.of[io.getquill.quat.Quat]

    class CustomExprMap extends ExprMap {
      def transform[TF](expr: Expr[TF])(using Type[TF])(using Quotes): Expr[TF] =
        expr match
          case '{ SerialHelper.fromSerialized[a](${ Expr(serial) }) } =>
            try {
              val actualAst = SerialHelper.fromSerialized[io.getquill.ast.Ast](serial)
              val astExpr = Lifter.NotSerializing.liftableAst(actualAst)

              // Need to cast or may fail on an internal typecheck
              '{ $astExpr.asInstanceOf[TF] }

            } catch {
              case e =>
                report.warning("Could Not Deserialize The AST During Investigation")
                ('{ io.getquill.ast.Constant.auto("Could Not Deserialize") }).asExprOf[TF]
            }

          // Quat.Generic blows this up for some reason
          case _ if (isQuat(expr)) => expr

          // Otherwise blows up and claims it can type Seq[Stuff] as _*
          case v @ Varargs(args) =>
            Try {
              val mappedArgs = args.map(arg => transformChildren(arg))
              '{ (${ Expr.ofList(mappedArgs) }).asInstanceOf[TF] }
            }.getOrElse(v)

          case other =>
            Try(transformChildren(other)).getOrElse(other)
    }

    try {
      new CustomExprMap().transform(input)
    } catch {
      case e =>
        report.warning("Could not Deserialize Ast Instances due to error:\n" + e)
        input
    }
  }
end DeserializeAstInstances

object ExprAccumulate {
  def apply[T: Type, ExpectedType](input: Expr[Any], recurseWhenMatched: Boolean = true)(matcher: PartialFunction[Expr[Any], T])(using Quotes): List[T] = {
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
          case e if e.getMessage.startsWith("Expr cast exception:") => // hello
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
        val found =
          if (!isQuat(expr))
            matcher.lift(expr) match
              case Some(result) =>
                buff += result
                true
              case None =>
                false
          else
            false

        // if we haven't found anything recurse
        // if we have found something and are told to continue recursion then continue recursion
        val continue = !found || recurseWhenMatched

        expr.asTerm match
          // Not including this causes exception "scala.tasty.reflect.ExprCastError: Expr: [ : Nothing]" in certain situations
          case Repeated(Nil, Inferred()) => expr
          case _ if (isQuat(expr))       => expr
          case _ if continue             => transformChildren[TF](expr)
          case _                         => expr
      }
    }

    accum.transform(input)
    buff.toList
  }
}
