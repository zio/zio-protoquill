package io.getquill.context.mirror

import io.getquill.generic._
import scala.reflect.ClassTag
import java.util.Date
import java.time.LocalDate
import java.util.UUID
import io.getquill.MappedEncoding

// TODO convert int back to index
trait MirrorEncoders extends EncodingDsl {
  override type PrepareRow = Row
  override type ResultRow  = Row
  type Encoder[T]          = MirrorEncoder[T]

  case class MirrorEncoder[T](encoder: EncoderMethod[T]) extends BaseEncoder[T] {
    override def apply(index: Int, value: T, row: PrepareRow, session: Session) =
      encoder(index, value, row, session)
  }

  def encoder[T]: MirrorEncoder[T] =
    MirrorEncoder((index: Int, value: T, row: PrepareRow, session: Session) => row.add(value))

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], e: Encoder[O]): Encoder[I] =
    MirrorEncoder((index: Int, value: I, row: PrepareRow, session: Session) => e(index, mapped.f(value), row, session))

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] =
    MirrorEncoder { (index: Int, value: Option[T], row: PrepareRow, session: Session) =>
      value match {
        case None    => row.add(None)
        case Some(v) => row.add(d(index, v, Row(), session).data.headOption)
      }
    }

  implicit val stringEncoder: Encoder[String]         = encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal] = encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean]       = encoder[Boolean]
  implicit val byteEncoder: Encoder[Byte]             = encoder[Byte]
  implicit val shortEncoder: Encoder[Short]           = encoder[Short]
  implicit val intEncoder: Encoder[Int]               = encoder[Int]
  implicit val longEncoder: Encoder[Long]             = encoder[Long]
  implicit val floatEncoder: Encoder[Float]           = encoder[Float]
  implicit val doubleEncoder: Encoder[Double]         = encoder[Double]
  implicit val byteArrayEncoder: Encoder[Array[Byte]] = encoder[Array[Byte]]
  implicit val dateEncoder: Encoder[Date]             = encoder[Date]
  implicit val localDateEncoder: Encoder[LocalDate]   = encoder[LocalDate]
  implicit val uuidEncoder: Encoder[UUID]             = encoder[UUID]
  implicit def nullEncoder: Encoder[Null]             = encoder[Null]
}
