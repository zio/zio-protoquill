package io.getquill.generic

import java.time.LocalDate
import java.util.Date
import java.util.UUID
import scala.reflect.ClassTag

//import io.getquill.context.sql.SqlContext

import scala.collection.compat._
import scala.language.higherKinds

// Different package in Scala2-Quill so need to do an import here
import io.getquill.MappedEncoding

trait ArrayCoreEncoder[T, PrepareRow] {
  def encode(arr: Array[T], idx: Int, row: PrepareRow): PrepareRow
}

trait ArrayCoreDecoder[ResultRow, T] {
  def decode(idx: Int, row: ResultRow): Array[T]
}

trait ArrayEncoding extends EncodingDsl {
  // self: SqlContext[_, _] =>

  type CBF[T, Col] = Factory[T, Col]

  implicit def arrayEncoder[T](implicit core: ArrayCoreEncoder[T, PrepareRow], ct: ClassTag[T]): Encoder[Array[T]]
  implicit def seqEncoder[T](implicit core: ArrayCoreEncoder[T, PrepareRow], ct: ClassTag[T]): Encoder[Seq[T]]
  implicit def listEncoder[T](implicit core: ArrayCoreEncoder[T, PrepareRow], ct: ClassTag[T]): Encoder[List[T]]
  implicit def setEncoder[T](implicit core: ArrayCoreEncoder[T, PrepareRow], ct: ClassTag[T]): Encoder[Set[T]]

  implicit def stringArrayCoreEncoder: ArrayCoreEncoder[String, PrepareRow]
  implicit def bigDecimalArrayCoreEncoder: ArrayCoreEncoder[BigDecimal, PrepareRow]
  implicit def booleanArrayCoreEncoder: ArrayCoreEncoder[Boolean, PrepareRow]
  implicit def byteArrayCoreEncoder: ArrayCoreEncoder[Byte, PrepareRow]
  implicit def shortArrayCoreEncoder: ArrayCoreEncoder[Short, PrepareRow]
  implicit def intArrayCoreEncoder: ArrayCoreEncoder[Int, PrepareRow]
  implicit def longArrayCoreEncoder: ArrayCoreEncoder[Long, PrepareRow]
  implicit def floatArrayCoreEncoder: ArrayCoreEncoder[Float, PrepareRow]
  implicit def doubleArrayCoreEncoder: ArrayCoreEncoder[Double, PrepareRow]
  implicit def dateArrayCoreEncoder: ArrayCoreEncoder[Date, PrepareRow]
  implicit def localDateArrayCoreEncoder: ArrayCoreEncoder[LocalDate, PrepareRow]
  implicit def uuidArrayCoreEncoder: ArrayCoreEncoder[UUID, PrepareRow]

  implicit def arrayStringDecoder[Col <: Seq[String]](implicit bf: CBF[String, Col]): Decoder[Col]
  implicit def arrayBigDecimalDecoder[Col <: Seq[BigDecimal]](implicit bf: CBF[BigDecimal, Col]): Decoder[Col]
  implicit def arrayBooleanDecoder[Col <: Seq[Boolean]](implicit bf: CBF[Boolean, Col]): Decoder[Col]
  implicit def arrayByteDecoder[Col <: Seq[Byte]](implicit bf: CBF[Byte, Col]): Decoder[Col]
  implicit def arrayShortDecoder[Col <: Seq[Short]](implicit bf: CBF[Short, Col]): Decoder[Col]
  implicit def arrayIntDecoder[Col <: Seq[Int]](implicit bf: CBF[Int, Col]): Decoder[Col]
  implicit def arrayLongDecoder[Col <: Seq[Long]](implicit bf: CBF[Long, Col]): Decoder[Col]
  implicit def arrayFloatDecoder[Col <: Seq[Float]](implicit bf: CBF[Float, Col]): Decoder[Col]
  implicit def arrayDoubleDecoder[Col <: Seq[Double]](implicit bf: CBF[Double, Col]): Decoder[Col]
  implicit def arrayDateDecoder[Col <: Seq[Date]](implicit bf: CBF[Date, Col]): Decoder[Col]
  implicit def arrayLocalDateDecoder[Col <: Seq[LocalDate]](implicit bf: CBF[LocalDate, Col]): Decoder[Col]
  implicit def arrayUuidDecoder[Col <: Seq[UUID]](implicit bf: Factory[UUID, Col]): Decoder[Col]
}
