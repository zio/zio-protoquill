package io.getquill.context.jdbc

import java.sql.{PreparedStatement, Timestamp, Date as SqlDate}
import java.sql.Types.*
import java.time.LocalDate
import java.util.Date
import java.util.UUID
import io.getquill.generic.ArrayEncoding
import io.getquill.generic.ArrayCoreEncoder
import scala.reflect.ClassTag

import scala.collection.compat.*

trait ArrayEncoders extends ArrayEncoding {
  self: JdbcContextTypes with Encoders =>

  implicit def arrayEncoder[T](implicit core: ArrayCoreEncoder[T, PreparedStatement], ct: ClassTag[T]): Encoder[Array[T]] =
    encoder(ARRAY, (idx, seq, row) => core.encode(seq, idx, row))

  implicit def seqEncoder[T](implicit core: ArrayCoreEncoder[T, PreparedStatement], ct: ClassTag[T]): Encoder[Seq[T]] =
    encoder(ARRAY, (idx, seq, row) => core.encode(seq, idx, row)).contramap { (seq: Seq[T]) => seq.toArray }

  implicit def indexedSeqEncoder[T](implicit core: ArrayCoreEncoder[T, PreparedStatement], ct: ClassTag[T]): Encoder[IndexedSeq[T]] =
    encoder(ARRAY, (idx, seq, row) => core.encode(seq, idx, row)).contramap { (seq: IndexedSeq[T]) => seq.toArray }

  implicit def listEncoder[T](implicit core: ArrayCoreEncoder[T, PreparedStatement], ct: ClassTag[T]): Encoder[List[T]] =
    encoder(ARRAY, (idx, seq, row) => core.encode(seq, idx, row)).contramap { (seq: List[T]) => seq.toArray }

  implicit def setEncoder[T](implicit core: ArrayCoreEncoder[T, PreparedStatement], ct: ClassTag[T]): Encoder[Set[T]] =
    encoder(ARRAY, (idx, seq, row) => core.encode(seq, idx, row)).contramap { (set: Set[T]) => set.toArray }

  implicit def vectorEncoder[T](implicit core: ArrayCoreEncoder[T, PreparedStatement], ct: ClassTag[T]): Encoder[Vector[T]] =
    encoder(ARRAY, (idx, seq, row) => core.encode(seq, idx, row)).contramap { (vec: Vector[T]) => vec.toArray }


  implicit def stringArrayCoreEncoder: ArrayCoreEncoder[String, PreparedStatement] = encodeRaw(VARCHAR)
  implicit def booleanArrayCoreEncoder: ArrayCoreEncoder[Boolean, PreparedStatement] = encodeRaw(BOOLEAN)
  implicit def byteArrayCoreEncoder: ArrayCoreEncoder[Byte, PreparedStatement] = encodeRaw(TINYINT)
  implicit def shortArrayCoreEncoder: ArrayCoreEncoder[Short, PreparedStatement] = encodeRaw(SMALLINT)
  implicit def intArrayCoreEncoder: ArrayCoreEncoder[Int, PreparedStatement] = encodeRaw(INTEGER)
  implicit def longArrayCoreEncoder: ArrayCoreEncoder[Long, PreparedStatement] = encodeRaw(BIGINT)
  implicit def floatArrayCoreEncoder: ArrayCoreEncoder[Float, PreparedStatement] = encodeRaw(FLOAT)
  implicit def doubleArrayCoreEncoder: ArrayCoreEncoder[Double, PreparedStatement] = encodeRaw(DOUBLE)
  implicit def dateArrayCoreEncoder: ArrayCoreEncoder[Date, PreparedStatement] = encodeRaw(TIMESTAMP)
  implicit def uuidArrayCoreEncoder: ArrayCoreEncoder[UUID, PreparedStatement] = encodeRaw("uuid")
  implicit def timestampArrayCoreEncoder: ArrayCoreEncoder[Timestamp, PreparedStatement] = encodeRaw(TIMESTAMP)
  implicit def localDateArrayCoreEncoder: ArrayCoreEncoder[LocalDate, PreparedStatement] =
    encodeMapped(parseJdbcType(DATE), { (ld: LocalDate) => SqlDate.valueOf(ld) })

  // i.e. need to convert to a java big-decimal before passing it to the JDBC Array's AnyRef type
  implicit def bigDecimalArrayCoreEncoder: ArrayCoreEncoder[BigDecimal, PreparedStatement] =
    encodeMapped(parseJdbcType(NUMERIC), _.bigDecimal)

  def encodeRaw[T](sqlType: String): ArrayCoreEncoder[T, PreparedStatement] =
    new ArrayCoreEncoder {
      def encode(arr: Array[T], idx: Int, row: PreparedStatement) = {
        row.setArray(idx, row.getConnection.createArrayOf(sqlType, arr.asInstanceOf[Array[AnyRef]]))
        row
      }
    }

  def encodeRaw[T](sqlType: Int): ArrayCoreEncoder[T, PreparedStatement] =
    encodeRaw(parseJdbcType(sqlType))

  def encodeMapped[T](sqlType: String, mapping: T => AnyRef): ArrayCoreEncoder[T, PreparedStatement] =
    new ArrayCoreEncoder {
      def encode(arr: Array[T], idx: Int, row: PreparedStatement) = {
        row.setArray(idx, row.getConnection.createArrayOf(sqlType, arr.map(mapping(_))))
        row
      }
    }

//  implicit def arrayStringEncoder0[Col <: Seq[String]]: Encoder[Col] = arrayRawEncoder[String, Col](VARCHAR)
//  implicit def arrayBigDecimalEncoder[Col <: Seq[BigDecimal]]: Encoder[Col] = arrayEncoder[BigDecimal, Col](parseJdbcType(NUMERIC), _.bigDecimal)
//  implicit def arrayBooleanEncoder[Col <: Seq[Boolean]]: Encoder[Col] = arrayRawEncoder[Boolean, Col](BOOLEAN)
//  implicit def arrayByteEncoder[Col <: Seq[Byte]]: Encoder[Col] = arrayRawEncoder[Byte, Col](TINYINT)
//  implicit def arrayShortEncoder[Col <: Seq[Short]]: Encoder[Col] = arrayRawEncoder[Short, Col](SMALLINT)
//  implicit def arrayIntEncoder[Col <: Seq[Int]]: Encoder[Col] = arrayRawEncoder[Int, Col](INTEGER)
//  implicit def arrayLongEncoder[Col <: Seq[Long]]: Encoder[Col] = arrayRawEncoder[Long, Col](BIGINT)
//  implicit def arrayFloatEncoder[Col <: Seq[Float]]: Encoder[Col] = arrayRawEncoder[Float, Col](FLOAT)
//  implicit def arrayDoubleEncoder[Col <: Seq[Double]]: Encoder[Col] = arrayRawEncoder[Double, Col](DOUBLE)
//  implicit def arrayDateEncoder[Col <: Seq[Date]]: Encoder[Col] = arrayRawEncoder[Date, Col](TIMESTAMP)
//  implicit def arrayTimestampEncoder[Col <: Seq[Timestamp]]: Encoder[Col] = arrayRawEncoder[Timestamp, Col](TIMESTAMP)
//  implicit def arrayLocalDateEncoder[Col <: Seq[LocalDate]]: Encoder[Col] = arrayEncoder[LocalDate, Col](parseJdbcType(DATE), SqlDate.valueOf)
//  implicit def arrayUuidEncoder[Col <: Seq[UUID]]: Encoder[Col] = arrayRawEncoder[UUID, Col]("uuid")

//  def arrayEncoder[T, Col <: Seq[T]](jdbcType: String, mapper: T => AnyRef): Encoder[Col] = {
//    encoder[Col](ARRAY, (idx: Index, seq: Col, row: PrepareRow) => {
//      val bf = implicitly[CBF[AnyRef, Array[AnyRef]]]
//      row.setArray(
//        idx,
//        row.getConnection.createArrayOf(
//          jdbcType,
//          seq.foldLeft(bf.newBuilder)((b, x) => b += mapper(x)).result()
//        )
//      )
//    })
//  }
//
//  /**
//   * Creates JDBC array encoder for type `T` which is already supported by database as array element.
//   *
//   * @param jdbcType JDBC specific type identification, may be various regarding to JDBC driver
//   * @tparam T element type
//   * @tparam Col seq type
//   * @return JDBC array encoder
//   */
//  def arrayRawEncoder[T, Col <: Seq[T]](jdbcType: String): Encoder[Col] =
//    arrayEncoder[T, Col](jdbcType, _.asInstanceOf[AnyRef])
//
//  /**
//   * Transform jdbcType int using `parseJdbcType` and calls overloaded method to create Encoder
//   *
//   * @param jdbcType java.sql.Types
//   * @see arrayRawEncoder(jdbcType: String)
//   * @see JdbcContext#parseJdbcType(jdbcType: String)
//   */
//  def arrayRawEncoder[T, Col <: Seq[T]](jdbcType: Int): Encoder[Col] =
//    arrayRawEncoder[T, Col](parseJdbcType(jdbcType))
}
