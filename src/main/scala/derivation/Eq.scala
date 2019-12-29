package derivation

import scala.deriving._
import scala.compiletime.erasedValue

trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.compiletime.{erasedValue, summonFrom}
  import compiletime._
  import scala.deriving._

  inline def tryEql[TT](x: TT, y: TT): Boolean = summonFrom {
    case eq: Eq[TT] => eq.eql(x, y)
  }

  inline def eqlElems[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](productElement[elem](x, n), productElement[elem](y, n)) &&
        eqlElems[elems1](n + 1)(x, y)
      case _: Unit =>
        true
    }

  inline def eqlCases[Alts](n: Int)(x: Any, y: Any, ord: Int): Boolean =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          summonFrom {
            case m: Mirror.ProductOf[`alt`] => eqlElems[m.MirroredElemTypes](0)(x, y)
          }
        else eqlCases[alts1](n + 1)(x, y, ord)
      case _: Unit =>
        false
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): Eq[T] = new Eq[T] {
    def eql(x: T, y: T): Boolean =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = m.ordinal(x)
          ord == m.ordinal(y) && eqlCases[m.MirroredElemTypes](0)(x, y, ord)
        case m: Mirror.ProductOf[T] =>
          eqlElems[m.MirroredElemTypes](0)(x, y)
      }
  }

  implicit object IntEq extends Eq[Int] {
    def eql(x: Int, y: Int) = x == y
  }

  implicit object StringEq extends Eq[String] {
    def eql(x: String, y: String) = x == y
  }
}