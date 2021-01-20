package io.getquill.dsl

import io.getquill.quoter._
import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline}


trait GenericColumnResolver[ResultRow] {
  def apply(resultRow: ResultRow, columnName: String): Int
}

trait GenericDecoder[ResultRow, T] {
  def apply(i: Int, rr: ResultRow):T
}

trait GenericRowTyper[ResultRow, Co] {
  def test(rr: ResultRow): ClassTag[_]
}

object GenericDecoder {

  // transparent 
  inline def summonRowTyper[ResultRow, Co]: Option[GenericRowTyper[ResultRow, Co]] =
    summonFrom {
      case det: GenericRowTyper[ResultRow, Co] => Some(det)
      case _ => None
    }

  // TODO Cache this return since it only needs to be done once?
  inline def columnResolver[ResultRow]: Option[GenericColumnResolver[ResultRow]] =
    summonFrom {
        case res: GenericColumnResolver[ResultRow] => Some(res)
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
  inline def resolveIndexOrFallback[ResultRow](originalIndex: Int, resultRow: ResultRow, fieldName: String) =
    columnResolver[ResultRow] match {
      case None => 
        //println(s"---------------- Using Original Index ${originalIndex} ----------------")
        originalIndex
      case Some(resolver) =>
        //println(s"---------------- Using Resolver Index '${fieldName}' -> ${resolver(resultRow, fieldName)} ----------------")
        resolver(resultRow, fieldName)
    }

  inline def chooseAndDecode[Fields <: Tuple, Elems <: Tuple, ResultRow, Co](rawIndex: Int, resultRow: ResultRow): Co = {
    inline erasedValue[(Fields, Elems)] match {
      case (_: (field *: fields), _: (tpe *: tpes)) =>
        // TODO Optimize, move this out of method
        // If we've matched on the type, decode the children and return. E.g. if Co is Shape, determinant[ResultRow, Shape]
        // may return Shape.Square. If the 'head' value of this list of Sum-Types is Shape.Square then we encode a square
        // and return it
        val tpeClass = summonInline[ClassTag[tpe]].runtimeClass
        // TODO Cache this or move it to upper level (i.e. to 'decode' to improve performance)
        val rowClass = 
          summonRowTyper[ResultRow, Co] match
            case Some(typer) => typer.test(resultRow).runtimeClass
            case None => throw new IllegalArgumentException(s"Cannot summon RowTyper for type: ${showType[Co]}")

        //println(s"-----------=============== GOT HERE =============== ${showType[tpe]} -----------")
        if (tpeClass.isAssignableFrom(rowClass)) {
            val decoded = summonAndDecode[tpe, ResultRow](rawIndex, resultRow)
            decoded.asInstanceOf[Co]
        } else {
            chooseAndDecode[fields, tpes, ResultRow, Co](rawIndex, resultRow)
        }
      case (_, _: EmptyTuple) => throw new IllegalArgumentException(s"Cannot resolve coproduct type for ${showType[Co]}")
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

  inline def decode[T, ResultRow](index: Int, resultRow: ResultRow) =
    summonFrom {
      case ev: Mirror.Of[T] =>
        inline ev match {
          case m: Mirror.ProductOf[T] =>
            val tup = decodeChildern[m.MirroredElemLabels, m.MirroredElemTypes, ResultRow](index, resultRow)
            m.fromProduct(tup.asInstanceOf[Product]).asInstanceOf[T]
          case m: Mirror.SumOf[T] =>
            columnResolver[ResultRow] match {
              case None => throw new IllegalArgumentException(s"Need column resolver for in order to be able to decode a coproduct but none exists for ${showType[ResultRow]}")
              case _ =>
            }
            // Try to get the mirror element and decode it
            val res = chooseAndDecode[m.MirroredElemLabels, m.MirroredElemTypes, ResultRow, T](index, resultRow)
            res
        }
    }

  inline def derived[T, ResultRow]: GenericDecoder[ResultRow, T] = 
    new GenericDecoder[ResultRow, T] {
      def apply(index: Int, resultRow: ResultRow): T = decode[T, ResultRow](index, resultRow)     
    }
}
