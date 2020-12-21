package io.getquill.context

import io.getquill._
import scala.quoted._
import miniquill.quoter.UnquoteMacro

object InsertMacro {
  inline def apply[T](inline value: T): Insert[T] = ${ applyImpl('value) }
  def applyImpl[T: Type](value: Expr[T])(using Quotes): Expr[Insert[T]] = {
    import quotes.reflect._
    // TODO If implicit insert meta, need to detect it and use it instead (i.e. with exclusions)
    
    // use the quoation macro to parse the value into a class expression
    // use that with (v) => (v) -> (class-value) to create a quoation
    // incorporate that into a new quotation, use the generated quotation's lifts and runtime lifts
    // the output value
    // use the Unquote macro to take it back to an 'Insert[T]'


    null
  }

}