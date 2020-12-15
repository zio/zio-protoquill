package derivation

import scala.deriving._
import scala.compiletime.erasedValue

trait CsvEncoder[T] {
  def encode(elem: T): String
}

object CsvEncoder {
  import scala.compiletime.{erasedValue, summonFrom}
  import compiletime._
  import scala.deriving._

  inline def encodeElem[T](elem: T): String = summonFrom {
    case encoder: CsvEncoder[T] => encoder.encode(elem)
  }

  inline def encodeElems[Elems <: Tuple](idx: Int)(value: Any): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) => 
        encodeElem[elem](productElement[elem](value, idx)) :: encodeElems[elems1](idx + 1)(value)
      case _ => Nil
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): CsvEncoder[T] = new CsvEncoder[T] {
    def encode(value: T): String = 
      inline ev match {
        case m: Mirror.ProductOf[T] =>
          encodeElems[m.MirroredElemTypes](0)(value).mkString(", ")
      }
  }

  given intEncoder: CsvEncoder[Int] with { def encode(value: Int) = value + "" }
  given stringEncoder: CsvEncoder[String] with { def encode(value: String) = value }
}
