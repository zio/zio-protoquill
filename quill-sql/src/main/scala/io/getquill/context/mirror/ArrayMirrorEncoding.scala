package io.getquill.context.mirror

import java.time.LocalDate
import java.util.Date
import java.util.UUID
import io.getquill.SqlMirrorContext
import io.getquill.generic.ArrayEncoding
import io.getquill.generic.ArrayCoreEncoder

import scala.collection.{Factory, mutable}
import scala.reflect.ClassTag

trait ArrayMirrorEncoding extends ArrayEncoding {
  this: MirrorEncoders with MirrorDecoders =>

  implicit def arrayEncoder[T](implicit core: ArrayCoreEncoder[T, Row], ct: ClassTag[T]): Encoder[Array[T]] = MirrorEncoder.ofArray[T](core).contramap { (arr: Array[T]) => arr }
  implicit def seqEncoder[T](implicit core: ArrayCoreEncoder[T, Row], ct: ClassTag[T]): Encoder[Seq[T]] = MirrorEncoder.ofArray[T](core).contramap { (arr: Seq[T]) => arr.toArray }
  implicit def indexedSeqEncoder[T](implicit core: ArrayCoreEncoder[T, Row], ct: ClassTag[T]): Encoder[IndexedSeq[T]] = MirrorEncoder.ofArray[T](core).contramap { (arr: IndexedSeq[T]) => arr.toArray }
  implicit def listEncoder[T](implicit core: ArrayCoreEncoder[T, Row], ct: ClassTag[T]): Encoder[List[T]] = MirrorEncoder.ofArray[T](core).contramap { (arr: List[T]) => arr.toArray }
  implicit def setEncoder[T](implicit core: ArrayCoreEncoder[T, Row], ct: ClassTag[T]): Encoder[Set[T]] = MirrorEncoder.ofArray[T](core).contramap { (arr: Set[T]) => arr.toArray }
  implicit def vectorEncoder[T](implicit core: ArrayCoreEncoder[T, Row], ct: ClassTag[T]): Encoder[Vector[T]] = MirrorEncoder.ofArray[T](core).contramap { (arr: Vector[T]) => arr.toArray }

  implicit def stringArrayCoreEncoder: ArrayCoreEncoder[String, Row] = arrayCore[String]
  implicit def bigDecimalArrayCoreEncoder: ArrayCoreEncoder[BigDecimal, Row] = arrayCore[BigDecimal]
  implicit def booleanArrayCoreEncoder: ArrayCoreEncoder[Boolean, Row] = arrayCore[Boolean]
  implicit def byteArrayCoreEncoder: ArrayCoreEncoder[Byte, Row] = arrayCore[Byte]
  implicit def shortArrayCoreEncoder: ArrayCoreEncoder[Short, Row] = arrayCore[Short]
  implicit def intArrayCoreEncoder: ArrayCoreEncoder[Int, Row] = arrayCore[Int]
  implicit def longArrayCoreEncoder: ArrayCoreEncoder[Long, Row] = arrayCore[Long]
  implicit def floatArrayCoreEncoder: ArrayCoreEncoder[Float, Row] = arrayCore[Float]
  implicit def doubleArrayCoreEncoder: ArrayCoreEncoder[Double, Row] = arrayCore[Double]
  implicit def dateArrayCoreEncoder: ArrayCoreEncoder[Date, Row] = arrayCore[Date]
  implicit def localDateArrayCoreEncoder: ArrayCoreEncoder[LocalDate, Row] = arrayCore[LocalDate]
  implicit def uuidArrayCoreEncoder: ArrayCoreEncoder[UUID, Row] = arrayCore[UUID]

  implicit def arrayStringDecoder[Col <: Seq[String]](implicit bf: CBF[String, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayBigDecimalDecoder[Col <: Seq[BigDecimal]](implicit bf: CBF[BigDecimal, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayBooleanDecoder[Col <: Seq[Boolean]](implicit bf: CBF[Boolean, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayByteDecoder[Col <: Seq[Byte]](implicit bf: CBF[Byte, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayShortDecoder[Col <: Seq[Short]](implicit bf: CBF[Short, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayIntDecoder[Col <: Seq[Int]](implicit bf: CBF[Int, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayLongDecoder[Col <: Seq[Long]](implicit bf: CBF[Long, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayFloatDecoder[Col <: Seq[Float]](implicit bf: CBF[Float, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayDoubleDecoder[Col <: Seq[Double]](implicit bf: CBF[Double, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayDateDecoder[Col <: Seq[Date]](implicit bf: CBF[Date, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayLocalDateDecoder[Col <: Seq[LocalDate]](implicit bf: CBF[LocalDate, Col]): Decoder[Col] = decoderUnsafe[Col]
  implicit def arrayUuidDecoder[Col <: Seq[UUID]](implicit bf: Factory[UUID, Col]): Decoder[Col] = decoderUnsafe[Col]
}
