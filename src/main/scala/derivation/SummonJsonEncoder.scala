package derivation

import scala.deriving._
import scala.quoted._

import scala.compiletime.{erasedValue, summonFrom}
import printer._


object SummonJsonEncoder {

  // Doesn't work because mirror is a function2?
  inline def encodeFromSummonedMirror[T](value: =>T): String = {
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        JsonEncoder.derived(m).encode(value)
    }
  }

  // ************ Just Attempting to mess around and see if I can get back the type tree ********
  inline def encodeAndMessAroundTerm[T](value: =>T): String = ${ encodeAndMessAroundTermImpl('value) }
  def encodeAndMessAroundTermImpl[T: Type](value: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect._
    val expr = '{
      given Mirror.Of[T] = null
      given JsonEncoder[T] = JsonEncoder.derived
    }
    //println( AstPrinter.astprint(Term.of(expr).showExtractors) )
    println(expr.show)

    '{"Hello"}
  }

  // https://github.com/lampepfl/dotty/issues/7853
  // inline def encodeAndMessAroundType[T](value: =>T): String = ${ encodeAndMessAroundTypeImpl('value) }
  // def encodeAndMessAroundTypeImpl[T](value: Expr[T])(using Quotes, t: Type[T]): Expr[String] = {
  //   import quotes.reflect._
  //   val mirrorExpr = Expr.summon[Mirror.Of[T]] match {
  //     case Some(mirror) => mirror
  //   }

  //   '{
  //     using JsonEncoder[$t] = JsonEncoder.derived($mirrorExpr)

  //     val encoder = summon[JsonEncoder[$t]]
  //     encoder.encode($value)
  //   }
  // }

  inline def encodeAndMessAround(value: =>ThePerson): String = ${ quoteAndMessAroundImpl('value) }
  def quoteAndMessAroundImpl(value: Expr[ThePerson])(using Quotes): Expr[String] = {
    '{
      given JsonEncoder[ThePerson] = JsonEncoder.derived
      given JsonEncoder[TheAddress] = JsonEncoder.derived
      val encoder = summon[JsonEncoder[ThePerson]]
      encoder.encode($value)
    }
  }
}
