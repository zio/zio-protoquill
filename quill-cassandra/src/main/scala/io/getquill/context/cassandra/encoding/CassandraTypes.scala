package io.getquill.context.cassandra.encoding

import java.lang.{ Boolean => JBoolean, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort, Byte => JByte }
import java.math.{ BigDecimal => JBigDecimal }
import java.nio.ByteBuffer
import java.util.{ Date, UUID }

import com.datastax.driver.core.LocalDate

/**
 * `CassandraTypes` contains implicit markers for already supported types by Cassandra.
 * Any of this type can be used in raw encoders/decoders as well as in collections encoding.
 * For custom types please use `MappedEncoding` as in `MappedTypes` trait for example.
 */
trait CassandraTypes extends CassandraMappedTypes {
  implicit val byteCassandraType: CassandraType[JByte] = CassandraType.of[JByte]
  implicit val shortCassandraType: CassandraType[JShort] = CassandraType.of[JShort]
  implicit val integerCassandraType: CassandraType[JInt] = CassandraType.of[JInt]
  implicit val longCassandraType: CassandraType[JLong] = CassandraType.of[JLong]
  implicit val floatCassandraType: CassandraType[JFloat] = CassandraType.of[JFloat]
  implicit val doubleCassandraType: CassandraType[JDouble] = CassandraType.of[JDouble]
  implicit val booleanCassandraType: CassandraType[JBoolean] = CassandraType.of[JBoolean]
  implicit val decimalCassandraType: CassandraType[JBigDecimal] = CassandraType.of[JBigDecimal]
  implicit val stringCassandraType: CassandraType[String] = CassandraType.of[String]
  implicit val byteBufferCassandraType: CassandraType[ByteBuffer] = CassandraType.of[ByteBuffer]
  implicit val uuidCassandraType: CassandraType[UUID] = CassandraType.of[UUID]
  implicit val dateCassandraType: CassandraType[Date] = CassandraType.of[Date]
  implicit val localDateCassandraType: CassandraType[LocalDate] = CassandraType.of[LocalDate]
}

/**
 * `MappedTypes` contains implicit `CassandraMapper` for Scala primitive/common types
 * which are not in relation with CassandraTypes but can be represented as ones.
 */
trait CassandraMappedTypes {

  implicit val encodeByte: CassandraMapper[Byte, JByte, MapperSide.Encode] = CassandraMapper.encode(byte2Byte)
  implicit val decodeByte: CassandraMapper[JByte, Byte, MapperSide.Decode] = CassandraMapper.decode(Byte2byte)

  implicit val encodeShort: CassandraMapper[Short, JShort, MapperSide.Encode] = CassandraMapper.encode(short2Short)
  implicit val decodeShort: CassandraMapper[JShort, Short, MapperSide.Decode] = CassandraMapper.decode(Short2short)

  implicit val encodeInt: CassandraMapper[Int, JInt, MapperSide.Encode] = CassandraMapper.encode(int2Integer)
  implicit val decodeInt: CassandraMapper[JInt, Int, MapperSide.Decode] = CassandraMapper.decode(Integer2int)

  implicit val encodeLong: CassandraMapper[Long, JLong, MapperSide.Encode] = CassandraMapper.encode(long2Long)
  implicit val decodeLong: CassandraMapper[JLong, Long, MapperSide.Decode] = CassandraMapper.decode(Long2long)

  implicit val encodeFloat: CassandraMapper[Float, JFloat, MapperSide.Encode] = CassandraMapper.encode(float2Float)
  implicit val decodeFloat: CassandraMapper[JFloat, Float, MapperSide.Decode] = CassandraMapper.decode(Float2float)

  implicit val encodeDouble: CassandraMapper[Double, JDouble, MapperSide.Encode] = CassandraMapper.encode(double2Double)
  implicit val decodeDouble: CassandraMapper[JDouble, Double, MapperSide.Decode] = CassandraMapper.decode(Double2double)

  implicit val encodeBoolean: CassandraMapper[Boolean, JBoolean, MapperSide.Encode] = CassandraMapper.encode(boolean2Boolean)
  implicit val decodeBoolean: CassandraMapper[JBoolean, Boolean, MapperSide.Decode] = CassandraMapper.decode(Boolean2boolean)

  implicit val encodeBigDecimal: CassandraMapper[BigDecimal, JBigDecimal, MapperSide.Encode] = CassandraMapper.encode(_.bigDecimal)
  implicit val decodeBigDecimal: CassandraMapper[JBigDecimal, BigDecimal, MapperSide.Decode] = CassandraMapper.decode(BigDecimal.apply)

  implicit val encodeByteArray: CassandraMapper[Array[Byte], ByteBuffer, MapperSide.Encode] = CassandraMapper.encode(ByteBuffer.wrap)
  implicit val decodeByteArray: CassandraMapper[ByteBuffer, Array[Byte], MapperSide.Decode] = CassandraMapper.decode(bb => {
    val b = new Array[Byte](bb.remaining())
    bb.get(b)
    b
  })
}
