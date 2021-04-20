package io.getquill.generic


import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}  
import io.getquill.MappedEncoding

trait LowPriorityImplicits { this: EncodingDsl =>
  // implicit inline def anyValEncoder[Cls <: AnyVal, V]: Encoder[Cls] =
  //   mappedEncoder[Cls, V](
  //     AnyValToValMacro[Cls, V],
  //     implicitly[Encoder[V]]
  //   )

  // implicit inline def anyValDecoder[Cls <: AnyVal, V]: Decoder[Cls] =
  //   mappedDecoder[V, Cls](
  //     ValToAnyValMacro[V, Cls],
  //     implicitly[Decoder[V]]
  //   )
}

trait EncodingDsl { self =>
  type PrepareRow
  type ResultRow
  //type Index = Int

  type EncoderMethod[T] = (Int, T, PrepareRow) => PrepareRow
  type DecoderMethod[T] = (Int, ResultRow) => T

  // Final Encoder/Decoder classes that Context implementations will use for their actual signatures
  // need to by subtypes GenericEncoder for encoder summoning to work from SqlContext where Encoders/Decoders
  // are defined only abstractly.
  type Encoder[T] <: GenericEncoder[T, PrepareRow]
  type Decoder[T] <: GenericDecoder[ResultRow, T]

  type AnyValEncoder[T] = Encoder[T]
  type AnyValDecoder[T] = Decoder[T]

  // Initial Encoder/Decoder classes that Context implementations will subclass for their
  // respective Encoder[T]/Decoder[T] implementations e.g. JdbcEncoder[T](...) extends BaseEncoder[T]
  type BaseEncoder[T] = GenericEncoder[T, PrepareRow]
  type BaseDecoder[T] = GenericDecoder[ResultRow, T]

  type ColumnResolver = GenericColumnResolver[ResultRow]
  type RowTyper[T] = GenericRowTyper[ResultRow, T]

  // For: Mapped := Foo(value: String), Base := String
  // Encoding follows: (MappedEncoding(Foo) => String) <=(contramap)= Encoder(Foo)
  implicit def mappedEncoder[Mapped, Base](implicit mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): Encoder[Mapped]

  // For: Base := String, Mapped := Foo(value: String)
  // Decoding follows: (MappedEncoding(String) => Foo) =(map)=> Decoder(Foo)
  implicit def mappedDecoder[Base, Mapped](implicit mapped: MappedEncoding[Base, Mapped], decoder: Decoder[Base]): Decoder[Mapped]

  protected def mappedBaseEncoder[Mapped, Base](mapped: MappedEncoding[Mapped, Base], encoder: EncoderMethod[Base]): EncoderMethod[Mapped] =
    (index, value, row) => encoder(index, mapped.f(value), row)
  protected def mappedBaseDecoder[Base, Mapped](mapped: MappedEncoding[Base, Mapped], decoder: DecoderMethod[Base]): DecoderMethod[Mapped] =
    (index, row) => mapped.f(decoder(index, row))

  implicit def anyValEncoder[Cls <: AnyVal]: Encoder[Cls] =
    MappedEncoderMaker(new AnyValEncoderContext[AnyValEncoder, Encoder, Cls] {
      def mappedBaseEncoder[Base](mapped: MappedEncoding[Cls, Base], encoder: Encoder[Base]): Encoder[Cls] =
        self.mappedEncoder(mapped, encoder)
    })

  implicit def anyValDecoder[Cls <: AnyVal]: Decoder[Cls] =
    MappedEncoderMaker(new AnyValEncoderContext[AnyValDecoder, Decoder, Cls] {
      def mappedBaseDecoder[Base](mapped: MappedEncoding[Base, Cls], decoder: Decoder[Base]): Decoder[Cls] =
        self.mappedDecoder(mapped, decoder)
    })

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
