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

trait Encoders extends EncodingDsl {
  this: JdbcContextTypes[_, _] =>

  // In Protoquill assuming indexes are Ints. Eventually need to generalize but not yet.
  // type Index = Int (Defined in JdbcRunContext)
  type Encoder[T] = JdbcEncoder[T]

  case class JdbcEncoder[T](sqlType: Int, encoder: EncoderMethod[T]) extends BaseEncoder[T] {
    override def apply(index: Index, value: T, row: PrepareRow, session: Session) = {
      println(s"-------------- Encoding: `${pprint(value).plainText}` at Index: ${index + 1}")
      encoder(index + 1, value, row, session)
    }
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
          case None    => integerBasedNullEncoder.encoder(index, d.sqlType, row, session)
        }
    )

  implicit val stringEncoder: Encoder[String] =
    encoder(Types.VARCHAR, (row: PreparedStatement) => (i: Index, t: String) =>  {
      println(s"========= Setting Index: ${i} to ${t} using string encoder")
      row.setString(i, t)
    })
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
  implicit val localDateEncoder: Encoder[LocalDate] =
    encoder(Types.DATE, (index, value, row) =>
      row.setDate(index, Date.valueOf(value), Calendar.getInstance(dateTimeZone)))
  implicit val localDateTimeEncoder: Encoder[LocalDateTime] =
    encoder(Types.TIMESTAMP, (index, value, row) =>
      row.setTimestamp(index, Timestamp.valueOf(value), Calendar.getInstance(dateTimeZone)))
}