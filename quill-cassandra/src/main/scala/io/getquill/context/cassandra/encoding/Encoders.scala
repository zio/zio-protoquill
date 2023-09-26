package io.getquill.context.cassandra.encoding

import io.getquill.context.cassandra.CassandraRowContext
import io.getquill.MappedEncoding

import java.nio.ByteBuffer
import java.time.{ Instant, LocalDate, LocalTime }
import java.util.{ Date, UUID }
import io.getquill.generic._
import io.getquill.context.UdtValueLookup
import io.getquill.context.cassandra.encoding.UdtEncoding
import com.datastax.oss.driver.api.core.data.UdtValue
import com.datastax.oss.driver.api.core.cql.Row
import com.datastax.oss.driver.api.core.cql.BoundStatement

trait CassandraEncoderMaker[Encoder[_], T] {
  def apply(e: (Int, T, BoundStatement, UdtValueLookup) => BoundStatement): Encoder[T]
}

trait CassandraDecoderMaker[Decoder[_], T] {
  def apply(e: (Int, Row, UdtValueLookup) => T): Decoder[T]
}

trait CassandraEncodeMapperMaker[Encoder[_], T] {
  def apply(f: (T, UdtValueLookup) => UdtValue): CassandraMapper[T, UdtValue, MapperSide.Encode]
}

trait CassandraDecodeMapperMaker[Encoder[_], T] {
  def apply(f: (UdtValue, UdtValueLookup) => T): CassandraMapper[UdtValue, T, MapperSide.Decode]
}

trait Encoders
extends CassandraRowContext
with EncodingDsl
with CollectionEncoders
with CassandraMapperConversions
with CassandraTypes
with UdtEncoding {

  type Encoder[T] = CassandraEncoder[T]

  case class CassandraEncoder[T](encoder: EncoderMethod[T]) extends BaseEncoder[T] {
    override def apply(index: Int, value: T, row: PrepareRow, session: Session) =
      encoder(index, value, row, session)
  }

  def encoder[T](e: (Int, T, PrepareRow, Session) => PrepareRow): Encoder[T] = CassandraEncoder(e)

  def encoder[T](f: PrepareRow => (Int, T) => PrepareRow): Encoder[T] =
    encoder((index, value, row, session) => f(row)(index, value))

  private[this] val nullEncoder: Encoder[Null] =
    encoder((index, _, row, _) => row.setToNull(index))

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] =
    encoder { (index, value, row, session) =>
      value match {
        case None    => nullEncoder(index, null, row, session)
        case Some(v) => d(index, v, row, session)
      }
    }

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] =
    CassandraEncoder(mappedBaseEncoder(mapped, encoder.encoder))

  implicit val stringEncoder: Encoder[String] = encoder(_.setString)
  implicit val bigDecimalEncoder: Encoder[BigDecimal] =
    encoder((index, value, row, _) => row.setBigDecimal(index, value.bigDecimal))
  implicit val booleanEncoder: Encoder[Boolean] = encoder(_.setBoolean)
  implicit val byteEncoder: Encoder[Byte] = encoder(_.setByte)
  implicit val shortEncoder: Encoder[Short] = encoder(_.setShort)
  implicit val intEncoder: Encoder[Int] = encoder(_.setInt)
  implicit val longEncoder: Encoder[Long] = encoder(_.setLong)
  implicit val floatEncoder: Encoder[Float] = encoder(_.setFloat)
  implicit val doubleEncoder: Encoder[Double] = encoder(_.setDouble)
  implicit val byteArrayEncoder: Encoder[Array[Byte]] =
    encoder((index, value, row, _) => row.setByteBuffer(index, ByteBuffer.wrap(value)))
  implicit val uuidEncoder: Encoder[UUID] = encoder(_.setUuid)
  implicit val timestampEncoder: Encoder[Instant] = encoder(_.setInstant)
  implicit val cassandraLocalTimeEncoder: Encoder[LocalTime] = encoder(_.setLocalTime)
  implicit val cassandraLocalDateEncoder: Encoder[LocalDate] = encoder(_.setLocalDate)
}
