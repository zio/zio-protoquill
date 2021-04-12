package io.getquill.util.debug

import scala.quoted._

object NewMac {
  inline def apply[T] = ${ newThisImpl[T] }
  def newThisImpl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect._
    val constructor = TypeRepr.of[T].typeSymbol.primaryConstructor

    val constructed =
      Apply(
      TypeApply(
        Select(New(TypeTree.of[T]), constructor),
        List(TypeTree.of[String], TypeTree.of[Int])
      ),
      List(Literal(StringConstant("Joe")), Literal(IntConstant(123)))
      )

    
    //Select(New(TypeTree.of[T]), constructor)

    constructed.asExprOf[T]
    //'{ ??? }
  }
}