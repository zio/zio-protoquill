package inlinecontext

import scala.quoted._


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
  //type Contextable[T] = (using qctx: QuoteContext, t: Type[T]) => Expr[String]
  //def summonAndReturnImpl[T](mc: Expr[MyContextTrait[T]]): Contextable[T] = {

  def summonAndReturnImpl[T](mc: Expr[MyContextTrait[T]])(using qctx: QuoteContext, t: Type[T]): Expr[String] = {
    val qctx = summon[QuoteContext]
    val t = summon[Type[T]]
    import qctx.tasty._
    val stuff = Expr.summon[Stuff[T]] match {
      case Some(value) => value
    }
    '{
      $stuff.doStuff + "-" + $mc.someLocalMethod
    }
  }
}