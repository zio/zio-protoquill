package inlinecontext

import scala.quoted._
import scala.quoted.matching._

trait Stuff[T] { def doStuff: String }
given Stuff[Int] = new Stuff[Int] { def doStuff: String = "IntStuff" }
given Stuff[String] = new Stuff[String] { def doStuff: String = "StringStuff" }



trait MyContextTrait[T] {
  inline def summonAndReturn: String = ${ MyContext.summonAndReturnImpl[T]('this) }
  def someLocalMethod: String
}

class MyContext[T] extends MyContextTrait[T] {
  def someLocalMethod: String = "hello"
}

object MyContext {
  def summonAndReturnImpl[T: Type](mc: Expr[MyContextTrait[T]])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{given, _}
    val stuff = summonExpr[Stuff[T]] match {
      case Some(value) => value
    }
    '{
      $stuff.doStuff + "-" + $mc.someLocalMethod
    }
  }
}