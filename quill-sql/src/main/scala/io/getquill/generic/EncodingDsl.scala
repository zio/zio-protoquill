package io.getquill.generic


import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}
import io.getquill.MappedEncoding
import io.getquill.generic.DecodingType

/**
 * Note that much of the implementation of anyValEncoder/anyValDecoder is a workaround for:
 * https://github.com/lampepfl/dotty/issues/12179#issuecomment-826294510
 *
 * Originally, the idea was to simply pass the `self` in `LowPriorityImplicits` directly
 * into the macro that creates the AnyValEncoders. That way, the implementation would be as simple as:
 * {{{
 *   trait LowPriorityImplicits { self: EncodingDsl =>
 *     implicit inline def anyValEncoder[Cls <: AnyVal]: Encoder[Cls] =
 *       new MappedEncoderMaker[Encoder, Cls](self)
 *   }
 * }}}
 * Then, the MappedEncoderMaker could just internally call `self.mappedEncoder(mapped, encoder)`
 * (where this `self` is the one that is passed in from the `LowPriorityImplicits`).
 *
 * Unfortunately however, because of Dotty#12179, this would create an implicit encoder which would
 * never be found. This created the need for the additional abstraction of AnyValEncoderContext and
 * AnyValDecoderContext which would define `makeMappedEncoder`/`makeMappedDecoder` stub methods
 * that the `LowPriorityImplicits` methods `anyValEncoder`/`anyValDecoder` could delegate the actual
 * encoding/decoding work into. Hopefully when Dotty#12179 is resolved all of this convoluted logic
 * can be removed and we can go back to the simpler implementation.
 */
trait LowPriorityImplicits { self: EncodingDsl =>

  implicit inline def anyValEncoder[Cls <: AnyVal]: Encoder[Cls] =
    MappedEncoderMaker[Encoder, Cls].apply(
      new AnyValEncoderContext[Encoder, Cls] {
        override def makeMappedEncoder[Base](mapped: MappedEncoding[Cls, Base], encoder: Encoder[Base]): Encoder[Cls] =
          self.mappedEncoder(mapped, encoder)
      })

  implicit inline def anyValDecoder[Cls <: AnyVal]: Decoder[Cls] =
    MappedDecoderMaker[Decoder, Cls].apply(
      new AnyValDecoderContext[Decoder, Cls] {
        override def makeMappedDecoder[Base](mapped: MappedEncoding[Base, Cls], decoder: Decoder[Base]): Decoder[Cls] =
          self.mappedDecoder(mapped, decoder)
    })
}

trait EncodingDsl extends LowPriorityImplicits { self => //extends LowPriorityImplicits
  type PrepareRow
  type ResultRow
  //type Index = Int

  type EncoderMethod[T] = (Int, T, PrepareRow) => PrepareRow
  type DecoderMethod[T] = (Int, ResultRow) => T

  // Final Encoder/Decoder classes that Context implementations will use for their actual signatures
  // need to by subtypes GenericEncoder for encoder summoning to work from SqlContext where Encoders/Decoders
  // are defined only abstractly.
  type Encoder[T] <: GenericEncoder[T, PrepareRow]
  type Decoder[T] <: GenericDecoder[ResultRow, T, DecodingType.Specific]

  // Initial Encoder/Decoder classes that Context implementations will subclass for their
  // respective Encoder[T]/Decoder[T] implementations e.g. JdbcEncoder[T](...) extends BaseEncoder[T]
  type BaseEncoder[T] = GenericEncoder[T, PrepareRow]
  type BaseDecoder[T] = GenericDecoder[ResultRow, T, DecodingType.Specific]

  type ColumnResolver = GenericColumnResolver[ResultRow]
  type RowTyper[T] = GenericRowTyper[ResultRow, T]

  // For: Mapped := Foo(value: String), Base := String
  // Encoding follows: (MappedEncoding(Foo) => String) <=(contramap)= Encoder(Foo)
  implicit def mappedEncoder  [Mapped, Base](implicit mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): Encoder[Mapped]

  // For: Base := String, Mapped := Foo(value: String)
  // Decoding follows: (MappedEncoding(String) => Foo) =(map)=> Decoder(Foo)
  implicit def mappedDecoder[Base, Mapped](implicit mapped: MappedEncoding[Base, Mapped], decoder: Decoder[Base]): Decoder[Mapped]

  protected def mappedBaseEncoder[Mapped, Base](mapped: MappedEncoding[Mapped, Base], encoder: EncoderMethod[Base]): EncoderMethod[Mapped] =
    (index, value, row) => encoder(index, mapped.f(value), row)
  protected def mappedBaseDecoder[Base, Mapped](mapped: MappedEncoding[Base, Mapped], decoder: DecoderMethod[Base]): DecoderMethod[Mapped] =
    (index, row) => mapped.f(decoder(index, row))

  // Define some standard encoders that all contexts should have
  implicit def stringEncoder: Encoder[String]
  implicit def bigDecimalEncoder: Encoder[BigDecimal]
  implicit def booleanEncoder: Encoder[Boolean]
  implicit def byteEncoder: Encoder[Byte]
  implicit def shortEncoder: Encoder[Short]
  implicit def intEncoder: Encoder[Int]
  implicit def longEncoder: Encoder[Long]
  implicit def doubleEncoder: Encoder[Double]
}
