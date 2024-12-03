package io.getquill.context.jdbc

import io.getquill.generic.{DecodingType, StandardCodec, GenericDecoder}

import java.time.{LocalDate, LocalDateTime}
import java.util
import java.util.{Calendar, TimeZone}
import scala.math.BigDecimal.javaBigDecimal2bigDecimal

// Needed as an import in Protoquill but not in Scala2 Quill. Not sure why
import io.getquill.MappedEncoding
import java.time.LocalTime
import java.time.ZonedDateTime
import java.time.OffsetDateTime
import java.time.Instant
import java.time.OffsetTime
import java.time.ZoneOffset

trait Decoders extends StandardCodec with JdbcContextTypes {

  case class JdbcDecoder[T](decoder: DecoderMethod[T]) extends BaseDecoder[T] { self =>
    def apply(index: Index, row: ResultRow, session: Session) =
      decoder(index + 1, row, session)

    override def map[R](f: T => R): Decoder[R] =
      JdbcDecoder((index: Index, row: ResultRow, session: Session) => f(self.apply(index, row, session)))
  }

  // If it's d: DecoderMethod then it thinks in decoder( {(index, row) => f(row)(index)} )
  // the {(index, row) => f(row)(index)} is wrong because that's a method with 2 things but
  // DecoderMethod[T] is a single thing
  def decoder[T](d: (Int, ResultRow, Session) => T): Decoder[T] =
    JdbcDecoder(d)

  def decoder[T](f: ResultRow => Index => T): Decoder[T] =
    decoder((index, row, session) => f(row)(index))

  implicit def optionDecoder[T](implicit d: BaseDecoderAny[T]): Decoder[Option[T]] =
    JdbcDecoder(
      (index, row, session) => {
        try {
          // According to the JDBC spec, we first need to read the object before `row.wasNull` works
          row.getObject(index)
          if (row.wasNull()) {
            None
          } else {
            Some(d(index, row, session))
          }
        } catch {
          case _: NullPointerException if row.wasNull() => None
        }
      }
    )

  implicit val sqlDateDecoder: Decoder[java.sql.Date] = decoder(_.getDate)
  implicit val sqlTimeDecoder: Decoder[java.sql.Time] = decoder(_.getTime)
  implicit val sqlTimestampDecoder: Decoder[java.sql.Timestamp] = decoder(_.getTimestamp)

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
}

trait BasicTimeDecoders extends Decoders {
  this: JdbcContextTypes =>

  implicit val localDateDecoder: Decoder[LocalDate] =
    decoder((index, row, session) =>
      row.getDate(index).toLocalDate)
  implicit val localTimeDecoder: Decoder[LocalTime] =
    decoder((index, row, session) =>
      row.getTime(index).toLocalTime)
  implicit val localDateTimeDecoder: Decoder[LocalDateTime] =
    decoder((index, row, session) =>
      row.getTimestamp(index).toLocalDateTime)

  implicit val zonedDateTimeDecoder: Decoder[ZonedDateTime] =
    decoder((index, row, session) =>
      ZonedDateTime.ofInstant(row.getTimestamp(index).toInstant, dateTimeZone.toZoneId))
  implicit val instantDecoder: Decoder[Instant] =
    decoder((index, row, session) =>
      row.getTimestamp(index).toInstant)

  implicit val offsetTimeDecoder: Decoder[OffsetTime] =
    decoder((index, row, session) => {
      val utcLocalTime = row.getTime(index).toLocalTime
      utcLocalTime.atOffset(ZoneOffset.UTC)
    })
  implicit val offsetDateTimeDecoder: Decoder[OffsetDateTime] =
    decoder((index, row, session) =>
      OffsetDateTime.ofInstant(row.getTimestamp(index).toInstant, dateTimeZone.toZoneId))
}

trait ObjectGenericTimeDecoders extends Decoders {
  this: JdbcContextTypes =>

  implicit val localDateDecoder: Decoder[LocalDate] =
    decoder((index, row, session) =>
      row.getObject(index, classOf[LocalDate]))
  implicit val localTimeDecoder: Decoder[LocalTime] =
    decoder((index, row, session) =>
      row.getObject(index, classOf[LocalTime]))
  implicit val localDateTimeDecoder: Decoder[LocalDateTime] =
    decoder((index, row, session) =>
      row.getObject(index, classOf[LocalDateTime]))

  implicit val zonedDateTimeDecoder: Decoder[ZonedDateTime] =
    decoder((index, row, session) =>
      row.getObject(index, classOf[OffsetDateTime]).toZonedDateTime)
  implicit val instantDecoder: Decoder[Instant] =
    decoder((index, row, session) =>
      row.getObject(index, classOf[OffsetDateTime]).toInstant)

  implicit val offsetTimeDecoder: Decoder[OffsetTime] =
    decoder((index, row, session) =>
      row.getObject(index, classOf[OffsetTime]))
  implicit val offsetDateTimeDecoder: Decoder[OffsetDateTime] =
    decoder((index, row, session) =>
      row.getObject(index, classOf[OffsetDateTime]))
}
