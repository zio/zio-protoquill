package io.getquill.dsl

import io.getquill.quoter._
import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline}


trait ColumnResolver[ResultRow] {
  def apply(resultRow: ResultRow, columnName: String): Int
}

trait GenericDecoder[ResultRow, T] {
  def apply(i: Int, rr: ResultRow):T
}

trait RowTyper[ResultRow, Co] {
  inline def test[T](rr: ResultRow): Boolean
}






// trait Example[ResultRow](using quotes: Quotes) {
//   import quotes.reflect._

//   given deter: Determinant[ResultRow, Shape]

//   given dec1: GenericDecoder[ResultRow, String]
//   given dec2: GenericDecoder[ResultRow, Int]

//   given sq1: GenericDecoder[ResultRow, Shape.Square] = GenericDecoder.derived
//   given cr1: GenericDecoder[ResultRow, Shape.Circle] = GenericDecoder.derived
//   // implicit inline def genDec[ResultRow, T]: GenericDecoder[T] = ${ GenericDecoder.derived[ResultRow, T] }

//   // Note: Can program this logic directly in the decoder!!!
//   // I.e. use the 'Determinant to do summoning'
//   // Need a column resolver since columns need to be retrieved by name
//   given (using resolver: ColumnResolver[ResultRow], deter: Determinant[ResultRow, Shape]): GenericDecoder[ResultRow, Shape] with {
//     def apply(i: Int, rr: ResultRow): Shape =
//       deter.apply(rr) match {
//         case '[Shape.Circle] => summon[GenericDecoder[ResultRow, Shape.Circle]](i, rr)
//         case '[Shape.Square] => summon[GenericDecoder[ResultRow, Shape.Square]](i, rr)
//       }

//       //resolver.apply(rr, "shapeType")
//   }

//   //given JsonEncoder[ThePerson] = JsonEncoder.derived
//   //given JsonEncoder[TheAddress] = JsonEncoder.derived
//   //inline def [T](x: =>T) === (y: =>T)(using eq: Eq[T]): Boolean = eq.eqv(x, y)
//   //implicit inline def eqGen[T]: Eq[T] = ${ Eq.derived[T] }
// }

object GenericDecoder {

  // Determine a row type for a coproduct Co
  // TODO Need good error message for user if this is not found
  transparent inline def summonRowTyper[ResultRow, Co] = // : RowTyper[ResultRow, Co]
    summonFrom {
      case det: RowTyper[ResultRow, Co] => det
      // TODO Better error via report.throwError if cant find it (or return None and error later?)
    }

  // inline def summonDeterminantIfMatches[ResultRow, Co, PossibleCo](rr: ResultRow): Option[Determinant[ResultRow, Co]] =
  //   val ct = summonDeterminant[ResultRow, Co].apply(rr)
  //   (erasedValue[PossibleCo], ct) match {
  //     case (_: ev, _: ct)
  //   }

  // TODO Cache this return since it only needs to be done once?
  inline def columnResolver[ResultRow]: Option[ColumnResolver[ResultRow]] =
    summonFrom {
        case res: ColumnResolver[ResultRow] => Some(res)
        case _ => None
    }

  inline def showType[T]: String = ${ showTypeImpl[T] }
  def showTypeImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].simplified.typeSymbol.name)
  }

  inline def summonAndDecode[T, ResultRow](index: Int, resultRow: ResultRow): T =
    summonFrom {
        case dec: GenericDecoder[ResultRow, T] => dec(index, resultRow)
        case _ => throw new IllegalArgumentException(s"Cannot find decoder for type: ${showType[T]}")
        // TODO Better error via report.throwError if cant find it (or return None and error later?)
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

  inline def chooseAndDecode[Fields <: Tuple, Elems <: Tuple, ResultRow, Co](rawIndex: Int, resultRow: ResultRow): Co = {
    inline erasedValue[(Fields, Elems)] match {
      case (_: (field *: fields), _: (tpe *: tpes)) =>
        // TODO Optimize, move this out of method
        // If we've matched on the type, decode the children and return. E.g. if Co is Shape, determinant[ResultRow, Shape]
        // may return Shape.Square. If the 'head' value of this list of Sum-Types is Shape.Square then we encode a square
        // and return it
        println(s"-----------=============== GOT HERE =============== ${showType[tpe]} -----------")
        if (summonRowTyper[ResultRow, Co].test[tpe](resultRow)) {
            val decoded = summonAndDecode[tpe, ResultRow](rawIndex, resultRow)
            decoded.asInstanceOf[Co]
        } else {
            chooseAndDecode[fields, tpes, ResultRow, Co](rawIndex, resultRow)
        }

        
        // val determinedCls = summonDeterminant[ResultRow, Co](resultRow)
        // if (determinedCls.runtimeClass.isAssignableFrom(tpeCls.runtimeClass)) {
        //   val decoded = summonAndDecode[tpe, ResultRow](rawIndex, resultRow)
        //   decoded.asInstanceOf[Co]
        // } else {
        //   chooseAndDecode[fields, tpes, ResultRow, Co](rawIndex, resultRow)
        // }

        // determinant[ResultRow, Co](resultRow)
        
        // match {
        //   case _: Det[head] => 
        //     val decoded = summonAndDecode[head, ResultRow](rawIndex, resultRow)
        //     decoded.asInstanceOf[Co]
        //   case _ => 
        //     chooseAndDecode[fields, tail, ResultRow, Co](rawIndex, resultRow)
        // }

      case (_, _: EmptyTuple) => throw new IllegalArgumentException(s"Cannot resolve coproduct type")
    }
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
                  case m: Mirror.SumOf[T] =>
                    // Try to get the mirror element and decode it
                    val res = chooseAndDecode[m.MirroredElemLabels, m.MirroredElemTypes, ResultRow, T](index, resultRow)
                    res
                }
          }     
    }
}
