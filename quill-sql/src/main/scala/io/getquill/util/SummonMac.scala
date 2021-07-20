package io.getquill.util

import scala.quoted._

// TODO Move into the testing code
trait Genie:
  def greet: String
trait SingleGenie extends Genie
object SingleGenie extends SingleGenie:
  def greet: String = "Hello!!!"

object SummonMac {
  inline def apply(): Unit = ${ applyImpl }
  def applyImpl(using Quotes): Expr[Unit] =
    import quotes.reflect._
    Expr.summon[Genie] match
      case Some(genie) =>
        val actualTypeRepr = genie.asTerm.tpe.widen
        println(s"Found Genie of type: ${Format.TypeRepr(actualTypeRepr)}")
        val actualType = actualTypeRepr.asType
        actualType match
          case '[t] =>
            val loaded =
              LoadModule.apply[t].getOrElse { report.throwError(s"Could not summon genie of type: ${Format.TypeOf[t]}") }.asInstanceOf[Genie]
            println("My Greeting Is: " + loaded.greet)
      case None =>
        println("Not found Genie")
    '{ () }
}