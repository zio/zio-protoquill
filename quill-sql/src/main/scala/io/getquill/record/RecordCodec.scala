package io.getquill.record

import scala.NamedTuple
import scala.NamedTuple.NamedTuple
import scala.compiletime._
import scala.deriving.Mirror
import io.getquill.generic.{GenericEncoder, GenericDecoder, DecodingType}

/**
 * Encoder/decoder derivation using NamedTuple type-level operations.
 *
 * Uses `summonInline` to materialize all encoders/decoders for a case class's
 * fields in a single traversal, replacing the per-field `Expr.summon` calls
 * in `GenericDecoder.scala` that are one of the main compilation bottlenecks.
 *
 * Usage:
 * {{{
 *   // Summon all encoders for Person's fields at compile time
 *   val encoders = RecordCodec.summonEncoders[Person, PrepareRow, Session]
 *   // Returns (Encoder[String], Encoder[Int]) for Person(name: String, age: Int)
 * }}}
 */
object RecordCodec {

  /**
   * Summon all encoders for the fields of case class C.
   */
  inline def summonEncoders[C, PrepareRow, Session](using m: Mirror.ProductOf[C]): Tuple = {
    summonEncodersFromTypes[m.MirroredElemTypes, PrepareRow, Session]
  }

  private inline def summonEncodersFromTypes[T <: Tuple, PrepareRow, Session]: Tuple = {
    inline erasedValue[T] match {
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        summonInline[GenericEncoder[head, PrepareRow, Session]] *: summonEncodersFromTypes[tail, PrepareRow, Session]
    }
  }

  /**
   * Summon all decoders for the fields of case class C.
   */
  inline def summonDecoders[C, ResultRow, Session](using m: Mirror.ProductOf[C]): Tuple = {
    summonDecodersFromTypes[m.MirroredElemTypes, ResultRow, Session]
  }

  private inline def summonDecodersFromTypes[T <: Tuple, ResultRow, Session]: Tuple = {
    inline erasedValue[T] match {
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        summonInline[GenericDecoder[ResultRow, Session, head, DecodingType.Specific]] *: summonDecodersFromTypes[tail, ResultRow, Session]
    }
  }

  /**
   * Extract a value from a case class as a tuple of its field values.
   */
  inline def toTuple[C <: Product](value: C)(using m: Mirror.ProductOf[C]): m.MirroredElemTypes = {
    Tuple.fromProductTyped(value)
  }
}
