package io.getquill.context.mirror

import io.getquill.dsl._
import scala.reflect.ClassTag


// TODO convert int back to index
trait MirrorEncoders extends EncodingDsl {
  override type PrepareRow = Row
  override type ResultRow = Row

  case class MirrorEncoder[T](encoder: Encoder[T]) extends Encoder[T] {
    override def apply(index: Int, value: T, row: PrepareRow) =
      encoder(index, value, row)
  }

  def encoder[T]: Encoder[T] = MirrorEncoder((index: Int, value: T, row: PrepareRow) => row.add(value))

  // implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], e: Encoder[O]): Encoder[I] =
  //   MirrorEncoder((index: Index, value: I, row: PrepareRow) => e(index, mapped.f(value), row))

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] =
    MirrorEncoder((index: Int, value: Option[T], row: PrepareRow) => {
      value match {
        case None    => row.add(None)
        case Some(v) => row.add(d(index, v, Row()).data.headOption)
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
  //implicit val dateEncoder: Encoder[Date] = encoder[Date]
  //implicit val localDateEncoder: Encoder[LocalDate] = encoder[LocalDate]
  //implicit val uuidEncoder: Encoder[UUID] = encoder[UUID]
}