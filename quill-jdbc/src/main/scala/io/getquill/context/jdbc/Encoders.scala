package io.getquill.context.jdbc

import java.sql.{ Date, Timestamp, Types }
import java.time.{ LocalDate, LocalDateTime }
import java.util.{ Calendar, TimeZone }
import java.{ sql, util }

// Needed as an import in Protoquill but not in Scala2 Quill. Not sure why
import io.getquill.MappedEncoding

// Need to add this to extend EncodingDsl which now defines Encoder[T] and Decoder[T]
// in terms of GenericEncoder/GenericDecoder. Will need to add GenericEncoder to quill-core-portable
// and redefine encoders in this way to get cross-portability
import io.getquill.generic._
import java.sql.PreparedStatement
import java.time.LocalTime
import java.time.ZonedDateTime
import java.time.Instant
import java.time.OffsetTime
import java.time.ZoneOffset
import java.time.OffsetDateTime

trait Encoders extends EncodingDsl {
  this: JdbcContextTypes[_, _] =>

  // In Protoquill assuming indexes are Ints. Eventually need to generalize but not yet.
  // type Index = Int (Defined in JdbcRunContext)
  type Encoder[T] = JdbcEncoder[T]

  case class JdbcEncoder[T](sqlType: Int, encoder: EncoderMethod[T]) extends BaseEncoder[T] {
    override def apply(index: Index, value: T, row: PrepareRow, session: Session) =
      encoder(index + 1, value, row, session)
  }

  def encoder[T](sqlType: Int, f: (Index, T, PrepareRow) => Unit): Encoder[T] =
    JdbcEncoder(sqlType, (index: Index, value: T, row: PrepareRow, session: Session) => {
      f(index, value, row)
      row
    })

  def encoder[T](sqlType: Int, f: PrepareRow => (Index, T) => Unit): Encoder[T] =
    encoder(sqlType, (index: Index, value: T, row: PrepareRow) => f(row)(index, value))

  override implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], e: Encoder[O]): Encoder[I] =
    JdbcEncoder(e.sqlType, mappedBaseEncoder(mapped, e.encoder))

  private[this] val integerBasedNullEncoder: Encoder[Int] = encoder(Types.INTEGER, _.setNull)

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] =
    JdbcEncoder(
      d.sqlType,
      (index, value, row, session) =>
        value match {
          case Some(v) => d.encoder(index, v, row, session)
          case None    => encoder(d.sqlType, (i, v, r) => r.setNull(index, d.sqlType))(index, 0, row, session)
        }
    )

  implicit val sqlDateEncoder: Encoder[java.sql.Date] =
    encoder(Types.DATE, (index, value, row) =>
      row.setDate(index, value))
  implicit val sqlTimeEncoder: Encoder[java.sql.Time] =
    encoder(Types.TIME, (index, value, row) =>
      row.setTime(index, value))
  implicit val sqlTimestampEncoder: Encoder[java.sql.Timestamp] =
    encoder(Types.TIMESTAMP, (index, value, row) =>
      row.setTimestamp(index, value))

  implicit val stringEncoder: Encoder[String] =
    encoder(Types.VARCHAR, (row: PreparedStatement) => (i: Index, t: String) =>  row.setString(i, t))
  implicit val nullEncoder: Encoder[Null] =
    encoder(Types.NULL, (row: PreparedStatement) => (i: Index, t: String) =>  row.setNull(i, Types.NULL))
  implicit val bigDecimalEncoder: Encoder[BigDecimal] =
    encoder(Types.NUMERIC, (index, value, row) => row.setBigDecimal(index, value.bigDecimal))
  implicit val byteEncoder: Encoder[Byte] = encoder(Types.TINYINT, _.setByte)
  implicit val shortEncoder: Encoder[Short] = encoder(Types.SMALLINT, _.setShort)
  implicit val intEncoder: Encoder[Int] = encoder(Types.INTEGER, _.setInt)
  implicit val longEncoder: Encoder[Long] = encoder(Types.BIGINT, _.setLong)
  implicit val floatEncoder: Encoder[Float] = encoder(Types.FLOAT, _.setFloat)
  implicit val doubleEncoder: Encoder[Double] = encoder(Types.DOUBLE, _.setDouble)
  implicit val byteArrayEncoder: Encoder[Array[Byte]] = encoder(Types.VARBINARY, _.setBytes)
  implicit val dateEncoder: Encoder[util.Date] =
    encoder(Types.TIMESTAMP, (index, value, row) =>
      row.setTimestamp(index, new sql.Timestamp(value.getTime), Calendar.getInstance(dateTimeZone)))
}

trait BasicTimeEncoders extends Encoders {
  this: JdbcContextTypes[_, _] =>

  implicit val localDateEncoder: Encoder[LocalDate] =
    encoder(Types.DATE, (index, value, row) =>
      row.setDate(index, java.sql.Date.valueOf(value)))
  implicit val localTimeEncoder: Encoder[LocalTime] =
    encoder(Types.TIME, (index, value, row) =>
      row.setTime(index, java.sql.Time.valueOf(value)))
  implicit val localDateTimeEncoder: Encoder[LocalDateTime] =
    encoder(Types.TIMESTAMP, (index, value, row) =>
      row.setTimestamp(index, java.sql.Timestamp.valueOf(value)))

  implicit val zonedDateTimeEncoder: Encoder[ZonedDateTime] =
    encoder(Types.TIMESTAMP_WITH_TIMEZONE, (index, value, row) =>
      row.setTimestamp(index, Timestamp.from(value.toInstant)))
  implicit val instantEncoder: Encoder[Instant] =
    encoder(Types.TIMESTAMP_WITH_TIMEZONE, (index, value, row) =>
      row.setTimestamp(index, Timestamp.from(value)))

  implicit val offsetTimeEncoder: Encoder[OffsetTime] =
    encoder(Types.TIME, (index, value, row) =>
      row.setTime(index, java.sql.Time.valueOf(value.withOffsetSameInstant(ZoneOffset.UTC).toLocalTime)))
  implicit val offsetDateTimeEncoder: Encoder[OffsetDateTime] =
    encoder(Types.TIMESTAMP_WITH_TIMEZONE, (index, value, row) =>
      row.setTimestamp(index, java.sql.Timestamp.from(value.toInstant)))
}

/** Encoders for reasonably implemented JDBC contexts that meet the 4.2 specification */
trait ObjectGenericTimeEncoders extends Encoders {
  this: JdbcContextTypes[_, _] =>

  protected def jdbcTypeOfLocalDate = Types.DATE
  protected def jdbcTypeOfLocalTime = Types.TIME
  protected def jdbcTypeOfLocalDateTime = Types.TIMESTAMP
  protected def jdbcTypeOfZonedDateTime = Types.TIMESTAMP_WITH_TIMEZONE

  protected def jdbcEncodeInstant(value: Instant): Any = value.atOffset(ZoneOffset.UTC)
  protected def jdbcTypeOfInstant = Types.TIMESTAMP_WITH_TIMEZONE
  protected def jdbcTypeOfOffsetTime = Types.TIME_WITH_TIMEZONE
  protected def jdbcTypeOfOffsetDateTime = Types.TIMESTAMP_WITH_TIMEZONE

  implicit val localDateEncoder: Encoder[LocalDate] =
    encoder(jdbcTypeOfLocalDate, (index, value, row) =>
      row.setObject(index, value, jdbcTypeOfLocalDate))
  implicit val localTimeEncoder: Encoder[LocalTime] =
    encoder(jdbcTypeOfLocalTime, (index, value, row) =>
      row.setObject(index, value, jdbcTypeOfLocalTime))
  implicit val localDateTimeEncoder: Encoder[LocalDateTime] =
    encoder(jdbcTypeOfLocalDateTime, (index, value, row) =>
      row.setObject(index, value, jdbcTypeOfLocalDateTime))

  implicit val zonedDateTimeEncoder: Encoder[ZonedDateTime] =
    encoder(jdbcTypeOfZonedDateTime, (index, value, row) =>
      row.setObject(index, value.toOffsetDateTime, jdbcTypeOfZonedDateTime))
  implicit val instantEncoder: Encoder[Instant] =
    encoder(jdbcTypeOfInstant, (index, value, row) =>
      row.setObject(index, jdbcEncodeInstant(value), jdbcTypeOfInstant))

  implicit val offsetTimeEncoder: Encoder[OffsetTime] =
    encoder(jdbcTypeOfOffsetTime, (index, value, row) =>
      row.setObject(index, value, jdbcTypeOfOffsetTime))
  implicit val offsetDateTimeEncoder: Encoder[OffsetDateTime] =
    encoder(jdbcTypeOfOffsetDateTime, (index, value, row) =>
      row.setObject(index, value, jdbcTypeOfOffsetDateTime))
}
