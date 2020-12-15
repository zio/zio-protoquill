package io.getquill

import scala.quoted._

/**
 * Adding the stringDummy and intDummy ensures that a Double definition error
 * does not occur.
 */
object Misc {
  trait StringDummy
  trait IntDummy
  implicit val stringDummy: StringDummy = new StringDummy {}
  implicit val intDummy: IntDummy = new IntDummy {}

  inline def foo(inline a: List[String])(implicit s: StringDummy): Unit = ${ fooImpl('a) }
  def fooImpl(a: Expr[List[String]])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    println(pprint.apply(a.asTerm))
    '{ () }
  }

  inline def foo(b: List[Int])(implicit i: IntDummy) = println(b)
}
