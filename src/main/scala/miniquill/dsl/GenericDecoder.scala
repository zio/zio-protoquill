package miniquill.dsl

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}



object GenericDecoder {
  inline def summonAndDecode[T, ResultRow](index: Int, resultRow: ResultRow): T =
    summonFrom {
        case dec: GenericDecoder[ResultRow, T] => dec(index, resultRow)
    }

  // inline def arity[Elems <: Tuple]: Int =
  //   inline erasedValue[Elems] match {
  //     case _: (head *: tail) => 1 + arity[tail]
  //     case _ => 0
  //   }

  type IsProduct[T <: Product] = T

  // TODO Handling of optionals. I.e. don't continue to decode child rows if any columns 
  // in the parent object are undefined (unless they are optional)
  inline def decodeChildern[Elems <: Tuple, ResultRow](index: Int, resultRow: ResultRow): Tuple =
    inline erasedValue[Elems] match {
      case _: (IsProduct[head] *: tail) =>
        val decodedHead = summonAndDecode[head, ResultRow](index, resultRow)
        val air = decodedHead.asInstanceOf[Product].productArity

        (decodedHead *: decodeChildern[tail, ResultRow](index + air, resultRow)) 
      case b: (head *: tail) =>
        (summonAndDecode[head, ResultRow](index, resultRow) *: decodeChildern[tail, ResultRow](index + 1, resultRow))
      case _ => ()
    }

  inline def derived[T, ResultRow]: GenericDecoder[ResultRow, T] = 
    summonFrom {
        case ev: Mirror.Of[T] =>
          new GenericDecoder[ResultRow, T] {
            def apply(index: Int, resultRow: ResultRow): T =
                inline ev match {
                  case m: Mirror.ProductOf[T] =>
                    val tup = decodeChildern[m.MirroredElemTypes, ResultRow](index, resultRow)
                    m.fromProduct(tup.asInstanceOf[Product]).asInstanceOf[T]
                }
          }     
    }
}

trait GenericDecoder[ResultRow, T] {
  def apply(i: Int, rr: ResultRow):T
}