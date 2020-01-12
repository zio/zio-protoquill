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

  

  inline def tuplizeChildren[Elems <: Tuple, ResultRow](index: Int, resultRow: ResultRow): Tuple =
    inline erasedValue[Elems] match {
      // <TODO ASK EPFL> how Product can be put into a parameter here
      case _: (Product *: tail) =>
        val (air, output) =
          inline erasedValue[Elems] match { 
            case _: (head *: tail) =>
              val ret = summonAndDecode[head, ResultRow](index, resultRow)
              val air = ret.asInstanceOf[Product].productArity
              (air, ret)
          }
        (output *: tuplizeChildren[tail, ResultRow](index + air, resultRow)) 
      case b: (head *: tail) =>
        (summonAndDecode[head, ResultRow](index, resultRow) *: tuplizeChildren[tail, ResultRow](index + 1, resultRow))
      case _ => ()
    }

  inline def derived[T, ResultRow](given ev: Mirror.Of[T]): GenericDecoder[ResultRow, T] = new GenericDecoder[ResultRow, T]() {
    def apply(index: Int, resultRow: ResultRow): T =
      inline ev match {
        case m: Mirror.ProductOf[T] =>
          val tup = tuplizeChildren[m.MirroredElemTypes, ResultRow](index, resultRow)
          m.fromProduct(tup.asInstanceOf[Product]).asInstanceOf[T]
      }
  }
}

trait GenericDecoder[ResultRow, T] {
  def apply(i: Int, rr: ResultRow):T
}