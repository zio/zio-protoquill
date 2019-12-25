package derivation

import scala.deriving._
import scala.compiletime.erasedValue

trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

trait JsonEncoder[T] {
  def encode(elem: T): String
}

object JsonEncoder {
  import scala.compiletime.{erasedValue, summonFrom}
  import compiletime._
  import scala.deriving._

  inline def encodeElem[T](elem: T): String = summonFrom {
    case encoder: JsonEncoder[T] => encoder.encode(elem)
  }

  inline def encodeElems[Elems <: Tuple](idx: Int)(value: Any): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) => 
        encodeElem[elem](productElement[elem](value, idx)) :: encodeElems[elems1](idx + 1)(value)
      case _ => Nil
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): JsonEncoder[T] = new JsonEncoder[T] {
    def encode(value: T): String = 
      inline ev match {
        case m: Mirror.SumOf[T] =>
          "not supporting this case yet"
        case m: Mirror.ProductOf[T] =>
          val elems = encodeElems[m.MirroredElemTypes](0)(value)
          val labels = value.asInstanceOf[Product].productElementNames
          val keyValues = labels.zip(elems).map((k, v) => s"$k: $v")
          "{" + (keyValues).mkString(", ") + "}"
      }
  }

  given listEncoder[T]: (encoder: JsonEncoder[T]) => JsonEncoder[List[T]] {
    def encode(list: List[T]) = s"[${ list.map(v => encoder.encode(v)).mkString(", ") }]"
  }

  given intEncoder: JsonEncoder[Int] {
    def encode(value: Int) = value + ""
  }

  given stringEncoder: JsonEncoder[String] {
    def encode(value: String) = value
  }
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