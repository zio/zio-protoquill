package derivation

import scala.quoted._
import scala.deriving._
import scala.annotation.StaticAnnotation
import printer.AstPrinter

object Autousing {
  // inline def autogiveEq[T](a: T, b: T)(using ev: deriving.Mirror.Of[T]): Boolean = ${ autogiveEqImpl('a, 'b)('ev) }
  // def autogiveEqImpl[T](a: Expr[T], b: Expr[T])(using ev: Expr[deriving.Mirror.Of[T]])(using qctx: QuoteContext): Expr[Boolean] = {
  //   '{
  //     using Eq[T] = Eq.derived
  //     val eqf = summon[Eq[T]]
  //     false
  //   }
  // }

  // TODO Can we get rid of the "using ev: deriving.Mirror.Of[T]" and use SummonFrom instead?

  inline def autogiveJsonEncoder[T](a: T)(using ev: deriving.Mirror.Of[T]): String = {
    given JsonEncoder[T] = JsonEncoder.derived
    val encoder = summon[JsonEncoder[T]]
    encoder.encode(a)
  }

  inline def autogiveEq[T](a: T, b: T)(using ev: deriving.Mirror.Of[T]): Boolean = {
    given Eq[T] = Eq.derived
    val eqf = summon[Eq[T]]
    eqf.eql(a, b)

    //using eq: Eq[T] = Eq.derived
    //eqf.eql(a, b)
  }
}
