package io.getquill

import io.getquill.generic.GenericEncoder
import scala.quoted._

case class Container[T, PrepareRow](encoder: GenericEncoder[T, PrepareRow])

object Macro {
  object SealedInline:
    def unapply[T: Type](using Quotes)(expr: Expr[T]) =
      import quotes.reflect._
      expr.asTerm match
        case Inlined(parent, defs, v) => Some((parent, defs, v.asExprOf[T]))
        case _ => None

  inline def matchContainer[T, PrepareRow](inline c: Container[T, PrepareRow]): Unit = ${ matchContainerImpl[T, PrepareRow]('c) }
  def matchContainerImpl[T: Type, PrepareRow: Type](c: Expr[Container[T, PrepareRow]])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    // c.asTerm.underlyingArgument.asExpr doesn't work either
    c.asTerm.underlyingArgument.asExpr match
      case SealedInline(_, _, '{ Container.apply[t, pr]($encoder) }) =>
        println(s"Matched and found inline: ${encoder.show}")
      case '{ Container.apply[t, pr]($encoder) } =>
        println(s"Matched and found: ${encoder.show}")
      case SealedInline(_, _, v) =>
        println(s"Not Matched! Found Inlined: ${v.show}")
      case _ =>
        println(s"Not Matched! Found: ${c.show}")

    '{ () }
  }

  inline def matchInjector[T, PrepareRow](inline c: InjectableEagerPlanter[T, PrepareRow]): Unit = ${ matchInjectorImpl[T, PrepareRow]('c) }
  def matchInjectorImpl[T: Type, PrepareRow: Type](c: Expr[InjectableEagerPlanter[T, PrepareRow]])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    c match
      case '{ InjectableEagerPlanter.apply[qta, prep]($liftValue, $encoder, ${Expr(uid: String)}) } =>
        println(s"Matched and found inject: ${encoder.show}")
      case _ =>
        println("Not Matched!")
    '{ () }
  }
}


object Example {
  // class Context {
  //   type PrepareRow
  //   type Encoder[T] <: GenericEncoder[T, PrepareRow]
  //   type BaseEncoder[T] = GenericEncoder[T, PrepareRow]
  //   implicit def arrayIntEncoder[Col <: Iterable[Int]]: Encoder[Col]
  // }
}
