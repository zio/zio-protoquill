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
  override type ResultRow = Row

  case class MirrorEncoder[T](encoder: EncoderMethod[T]) extends BaseEncoder[T] {
    override val sqlType = 0 // unused
    override def apply(index: Int, value: T, row: PrepareRow, session: Session) =
      encoder(index, value, row, session)
    override def contramap[U](f: U => T): MirrorEncoder[U] =
      MirrorEncoder((index: Int, value: U, row: PrepareRow, session: Session) => encoder(index, f(value), row, session))
  }
  object MirrorEncoder {
    def ofArray[T](core: ArrayCoreEncoder[T, PrepareRow]): MirrorEncoder[Array[T]] =
      MirrorEncoder((index: Int, value: Array[T], row: PrepareRow, session: Session) => core.encode(value, index, row))
  }

  case class MirrorArrayCoreEncoder[T](f: (index: Int, value: Array[T], row: PrepareRow) => PrepareRow) extends ArrayCoreEncoder[T, PrepareRow] {
    def encode(arr: Array[T], idx: Int, row: PrepareRow): PrepareRow = f(idx, arr, row)
  }

  def encoder[T]: MirrorEncoder[T] = MirrorEncoder((index: Int, value: T, row: PrepareRow, session: Session) => row.add(value))
  def arrayCore[T]: ArrayCoreEncoder[T, PrepareRow] = MirrorArrayCoreEncoder[T]((index, value, row) => row.add(value))

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] =
    MirrorEncoder((index: Int, value: Option[T], row: PrepareRow, session: Session) => {
      value match {
        case None    => row.add(None)
        case Some(v) => row.add(d(index, v, Row(), session).data.headOption)
      }
    })

  implicit val stringEncoder: Encoder[String] = encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal] = encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean] = encoder[Boolean]
  implicit val byteEncoder: Encoder[Byte] = encoder[Byte]
  implicit val shortEncoder: Encoder[Short] = encoder[Short]
  implicit val intEncoder: Encoder[Int] = encoder[Int]
  implicit val longEncoder: Encoder[Long] = encoder[Long]
  implicit val floatEncoder: Encoder[Float] = encoder[Float]
  implicit val doubleEncoder: Encoder[Double] = encoder[Double]
  implicit val byteArrayEncoder: Encoder[Array[Byte]] = encoder[Array[Byte]]
  implicit val dateEncoder: Encoder[Date] = encoder[Date]
  implicit val localDateEncoder: Encoder[LocalDate] = encoder[LocalDate]
  implicit val uuidEncoder: Encoder[UUID] = encoder[UUID]
  implicit def nullEncoder: Encoder[Null] = encoder[Null]
}
