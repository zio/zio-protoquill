package derivation

import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
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
  def encodeAndMessAroundTermImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given _}
    val expr = '{
      given Mirror.Of[T] = null
      given JsonEncoder[T] = JsonEncoder.derived
    }
    //println( AstPrinter.astprint(expr.unseal.showExtractors) )
    println(expr.show)

    '{"Hello"}
  }

  // https://github.com/lampepfl/dotty/issues/7853
  // inline def encodeAndMessAroundType[T](value: =>T): String = ${ encodeAndMessAroundTypeImpl('value) }
  // def encodeAndMessAroundTypeImpl[T](value: Expr[T])(given qctx: QuoteContext, t: Type[T]): Expr[String] = {
  //   import qctx.tasty.{_, given _}
  //   val mirrorExpr = summonExpr[Mirror.Of[T]] match {
  //     case Some(mirror) => mirror
  //   }

  //   '{
  //     given JsonEncoder[$t] = JsonEncoder.derived($mirrorExpr)

  //     val encoder = summon[JsonEncoder[$t]]
  //     encoder.encode($value)
  //   }
  // }

  inline def encodeAndMessAround(value: =>ThePerson): String = ${ quoteAndMessAroundImpl('value) }
  def quoteAndMessAroundImpl(value: Expr[ThePerson])(given qctx: QuoteContext): Expr[String] = {
    '{
      given JsonEncoder[ThePerson] = JsonEncoder.derived
      given JsonEncoder[TheAddress] = JsonEncoder.derived
      val encoder = summon[JsonEncoder[ThePerson]]
      encoder.encode($value)
    }
  }
}
