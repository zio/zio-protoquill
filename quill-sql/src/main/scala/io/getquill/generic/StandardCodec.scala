package io.getquill.generic

import scala.reflect.ClassTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{erasedValue, summonFrom}
import io.getquill.{MappedEncoding, ProductDecoders, toDecoder, toEncoder}
import io.getquill.context.mirror.MirrorSession
import io.getquill.generic.DecodingType

import java.time.LocalDate
import java.util.{Date, UUID}

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
trait LowPriorityImplicits { self: StandardCodec =>

  // The `implicit ev...` clauses here are better for implicits resolution errors than type boundries
  // because you get a "no implicit values were found MappedEncoding[MyClass]" error
  // instead of a "could not summon anyValEncoder for MyClass because it was not instances of AnyVal" error
  // in most cases the latter is big red herrig because AnyVal is not frequently used in Scala 3
  // due to the presence of opaque types and other constructs.
  implicit inline def anyValEncoder[Cls](implicit ev: Cls <:< AnyVal): Encoder[Cls] =
    MappedEncoderMaker[Encoder, Cls].apply(
      new AnyValEncoderContext[Encoder, Cls] {
        override def makeMappedEncoder[Base](mapped: MappedEncoding[Cls, Base], encoder: Encoder[Base]): Encoder[Cls] = {
          mapped.toEncoder[PrepareRow, Session](encoder)
        }
      }
    )

  implicit inline def anyValDecoder[Cls](implicit ev: Cls <:< AnyVal): Decoder[Cls] =
    MappedDecoderMaker[Decoder, Cls].apply(
      new AnyValDecoderContext[Decoder, Cls] {
        override def makeMappedDecoder[Base](mapped: MappedEncoding[Base, Cls], decoder: Decoder[Base]): Decoder[Cls] = {
          mapped.toDecoder[ResultRow, Session](decoder)
        }
      }
    )
}

// Generic null checker does not need to access the session but it needs to be typed on it for the context
// to know which one to summon.
trait GenericNullChecker[ResultRow, Session] {
  def apply(columnIndex: Int, resultRow: ResultRow): Boolean
}


trait DatabaseVerbs {
  type PrepareRow
  type ResultRow
  type Session
  type NullChecker
}

trait StandardCodec extends ProductDecoders with DatabaseVerbs with LowPriorityImplicits { self => // extends LowPriorityImplicits
  // type Index = Int

  type EncoderMethod[T] = (Int, T, PrepareRow, Session) => PrepareRow
  type DecoderMethod[T] = (Int, ResultRow, Session) => T

  // Final Encoder/Decoder classes that Context implementations will use for their actual signatures
  // need to by subtypes GenericEncoder for encoder summoning to work from SqlContext where Encoders/Decoders
  // are defined only abstractly.
  type Encoder[T] = GenericEncoder[T, PrepareRow, Session]
  type Decoder[T] = GenericDecoder[ResultRow, Session, T, DecodingType.Leaf]
  type NullChecker = GenericNullChecker[ResultRow, Session]

  // Initial Encoder/Decoder classes that Context implementations will subclass for their
  // respective Encoder[T]/Decoder[T] implementations e.g. JdbcEncoder[T](...) extends BaseEncoder[T]
  // TODO Don't need this anymore, Encoder is enough
  type BaseEncoder[T] = GenericEncoder[T, PrepareRow, Session]
  type BaseDecoder[T] = GenericDecoder[ResultRow, Session, T, DecodingType.Leaf]
  // Since sometimes you need a decoder to depend on another that could ge DecodingType.Specific or Generic (e.g. optionDecoder)
  // need to have a class that covers both options
  type BaseDecoderAny[T] = GenericDecoder[ResultRow, Session, T, _]

  type ColumnResolver = GenericColumnResolver[ResultRow]
  type RowTyper[T] = GenericRowTyper[ResultRow, T]

  implicit val nullChecker: NullChecker

  implicit def optionDecoder[T](implicit d: BaseDecoderAny[T]): Decoder[Option[T]]
  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]]

  implicit val stringDecoder: Decoder[String]
  implicit val bigDecimalDecoder: Decoder[BigDecimal]
  implicit val booleanDecoder: Decoder[Boolean]
  implicit val byteDecoder: Decoder[Byte]
  implicit val shortDecoder: Decoder[Short]
  implicit val intDecoder: Decoder[Int]
  implicit val longDecoder: Decoder[Long]
  implicit val floatDecoder: Decoder[Float]
  implicit val doubleDecoder: Decoder[Double]
  implicit val byteArrayDecoder: Decoder[Array[Byte]]
  implicit val dateDecoder: Decoder[Date]
  implicit val localDateDecoder: Decoder[LocalDate]
  implicit val uuidDecoder: Decoder[UUID]

  implicit val stringEncoder: Encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean]
  implicit val byteEncoder: Encoder[Byte]
  implicit val shortEncoder: Encoder[Short]
  implicit val intEncoder: Encoder[Int]
  implicit val longEncoder: Encoder[Long]
  implicit val floatEncoder: Encoder[Float]
  implicit val doubleEncoder: Encoder[Double]
  implicit val byteArrayEncoder: Encoder[Array[Byte]]
  implicit val dateEncoder: Encoder[Date]
  implicit val localDateEncoder: Encoder[LocalDate]
  implicit val uuidEncoder: Encoder[UUID]
}
