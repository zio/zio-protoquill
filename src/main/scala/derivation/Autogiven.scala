package derivation

import scala.quoted._
import scala.annotation.StaticAnnotation

object Autogiven {
  // inline def autogiveEq[T](a: T, b: T)(given ev: deriving.Mirror.Of[T]): Boolean = ${ autogiveEqImpl('a, 'b)('ev) }
  // def autogiveEqImpl[T](a: Expr[T], b: Expr[T])(given ev: Expr[deriving.Mirror.Of[T]])(given qctx: QuoteContext): Expr[Boolean] = {
  //   '{
  //     given Eq[T] = Eq.derived
  //     val eqf = summon[Eq[T]]
  //     false
  //   }
  // }

  inline def autogiveEq[T](a: T, b: T)(given ev: deriving.Mirror.Of[T]): Boolean = {
    given Eq[T] = Eq.derived
    val eqf = summon[Eq[T]]
    eqf.eql(a, b)
  }
  
}

