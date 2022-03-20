package io.getquill.context.jdbc

import java.time.{ LocalDate, LocalDateTime }
import java.util
import java.util.{ Calendar, TimeZone }

import scala.math.BigDecimal.javaBigDecimal2bigDecimal

// Needed as an import in Protoquill but not in Scala2 Quill. Not sure why
import io.getquill.MappedEncoding

trait Decoders {
  this: JdbcContextTypes[_, _] =>

  // In Protoquill assuming indexes are Ints. Eventually need to generalize but not yet.
  // type Index = Int (Defined in JdbcRunContext)
  type Decoder[T] = JdbcDecoder[T]

  case class JdbcDecoder[T](decoder: DecoderMethod[T]) extends BaseDecoder[T] {
    def apply(index: Index, row: ResultRow, session: Session) =
      decoder(index + 1, row, session)
  }

  // If it's d: DecoderMethod then it thinks in decoder( {(index, row) => f(row)(index)} )
  // the {(index, row) => f(row)(index)} is wrong because that's a method with 2 things but
  // DecoderMethod[T] is a single thing
  def decoder[T](d: (Int, ResultRow, Session) => T): Decoder[T] =
    JdbcDecoder(d)

  def decoder[T](f: ResultRow => Index => T): Decoder[T] =
    decoder((index, row, session) => f(row)(index))

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: Decoder[I]): Decoder[O] =
    JdbcDecoder(mappedBaseDecoder(mapped, d.decoder))

  implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] =
    JdbcDecoder(
      (index, row, session) => {
        try {
          // According to the JDBC spec, we first need to read the object before `row.wasNull` works
          row.getObject(index)
          if (row.wasNull()) {
            None
          } else {
            Some(d.decoder(index, row, session))
          }
        } catch {
          case _: NullPointerException if row.wasNull() => None
        }
      }
    )

  implicit val stringDecoder: Decoder[String] = decoder(_.getString)
  implicit val bigDecimalDecoder: Decoder[BigDecimal] =
    decoder((index, row, session) =>
      row.getBigDecimal(index))
  implicit val byteDecoder: Decoder[Byte] = decoder(_.getByte)
  implicit val shortDecoder: Decoder[Short] = decoder(_.getShort)
  implicit val intDecoder: Decoder[Int] = decoder(_.getInt)
  implicit val longDecoder: Decoder[Long] = decoder(_.getLong)
  implicit val floatDecoder: Decoder[Float] = decoder(_.getFloat)
  implicit val doubleDecoder: Decoder[Double] = decoder(_.getDouble)
  implicit val byteArrayDecoder: Decoder[Array[Byte]] = decoder(_.getBytes)
  implicit val dateDecoder: Decoder[util.Date] =
    decoder((index, row, session) =>
      new util.Date(row.getTimestamp(index, Calendar.getInstance(dateTimeZone)).getTime))
  implicit val localDateDecoder: Decoder[LocalDate] =
    decoder((index, row, session) =>
      row.getDate(index, Calendar.getInstance(dateTimeZone)).toLocalDate)
  implicit val localDateTimeDecoder: Decoder[LocalDateTime] =
    decoder((index, row, session) =>
      row.getTimestamp(index, Calendar.getInstance(dateTimeZone)).toLocalDateTime)
}
