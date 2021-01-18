package io.getquill.dsl

import io.getquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom}


trait ColumnResolver[ResultRow] {
  def apply(resultRow: ResultRow, columnName: String): Int
}

trait GenericDecoder[ResultRow, T] {
  def apply(i: Int, rr: ResultRow):T
}

// trait CoproductDeterminant[ResultRow, T] {

// }

// trait Example[ResultRow] {
  
//   enum Shape(id: String):
//     case Square(id: String, width: Int, height: Int) extends Shape(id)
//     case Circle(id: String, radius: Int) extends Shape(id)

//   given GenericDecoder[ResultRow, Shape.Square] = GenericDecoder.derived
//   given GenericDecoder[ResultRow, Shape.Circle] = GenericDecoder.derived

//   // Need a column resolver since columns need to be retrieved by name
//   given [ResultRow, Shape](using resolver: ColumnResolver[ResultRow]): GenericDecoder[ResultRow, Shape] with {
//     def apply(i: Int, rr: ResultRow): Shape =
//       resolver.apply(rr, "shapeType")
//   }

//   //given JsonEncoder[ThePerson] = JsonEncoder.derived
//   //given JsonEncoder[TheAddress] = JsonEncoder.derived
//   //inline def [T](x: =>T) === (y: =>T)(using eq: Eq[T]): Boolean = eq.eqv(x, y)
//   //implicit inline def eqGen[T]: Eq[T] = ${ Eq.derived[T] }
// }

object GenericDecoder {

  // TODO Cache this return since it only needs to be done once?
  inline def columnResolver[ResultRow]: Option[ColumnResolver[ResultRow]] =
    summonFrom {
        case res: ColumnResolver[ResultRow] => Some(res)
        case _ => None
    }

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

  // If a columnName -> columns index is available, use that. It is necessary for coproducts
  def resolveIndexOrFallback[ResultRow](originalIndex: Int, resultRow: ResultRow, fieldName: String) =
    columnResolver match {
      case None => originalIndex
      case Some(resolver) => resolver(resultRow, fieldName)
    }

  // TODO Handling of optionals. I.e. don't continue to decode child rows if any columns 
  // in the parent object are undefined (unless they are optional)
  inline def decodeChildern[Fields <: Tuple, Elems <: Tuple, ResultRow](rawIndex: Int, resultRow: ResultRow): Tuple =
    inline erasedValue[(Fields, Elems)] match {
      case (_: (field *: fields), _: (IsProduct[head] *: tail)) =>
        val index = resolveIndexOrFallback(rawIndex, resultRow, constValue[field].toString)
        val decodedHead = summonAndDecode[head, ResultRow](index, resultRow)
        val air = decodedHead.asInstanceOf[Product].productArity
        (decodedHead *: decodeChildern[fields, tail, ResultRow](index + air, resultRow)) 

      case (_: (field *: fields), _: (head *: tail)) =>
        val index = resolveIndexOrFallback(rawIndex, resultRow, constValue[field].toString)
        (summonAndDecode[head, ResultRow](index, resultRow) *: decodeChildern[fields, tail, ResultRow](index + 1, resultRow))

      case (_, _: EmptyTuple) => EmptyTuple
    }

  inline def derived[T, ResultRow]: GenericDecoder[ResultRow, T] = 
    summonFrom {
        case ev: Mirror.Of[T] =>
          new GenericDecoder[ResultRow, T] {
            def apply(index: Int, resultRow: ResultRow): T =
                inline ev match {
                  case m: Mirror.ProductOf[T] =>
                    val tup = decodeChildern[m.MirroredElemLabels, m.MirroredElemTypes, ResultRow](index, resultRow)
                    m.fromProduct(tup.asInstanceOf[Product]).asInstanceOf[T]
                }
          }     
    }
}
