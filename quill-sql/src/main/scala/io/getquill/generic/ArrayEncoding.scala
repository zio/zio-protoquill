package io.getquill.generic

import java.time.LocalDate
import java.util.Date

//import io.getquill.context.sql.SqlContext

import scala.collection.compat._
import scala.language.higherKinds

// Different package in Scala2-Quill so need to do an import here
import io.getquill.MappedEncoding

trait ArrayEncoding extends EncodingDsl {
  //self: SqlContext[_, _] =>

  type CBF[T, Col] = Factory[T, Col]

  implicit def arrayStringEncoder[Col <: Seq[String]]: ContextEncoder[Col]
  implicit def arrayBigDecimalEncoder[Col <: Seq[BigDecimal]]: ContextEncoder[Col]
  implicit def arrayBooleanEncoder[Col <: Seq[Boolean]]: ContextEncoder[Col]
  implicit def arrayByteEncoder[Col <: Seq[Byte]]: ContextEncoder[Col]
  implicit def arrayShortEncoder[Col <: Seq[Short]]: ContextEncoder[Col]
  implicit def arrayIntEncoder[Col <: Seq[Int]]: ContextEncoder[Col]
  implicit def arrayLongEncoder[Col <: Seq[Long]]: ContextEncoder[Col]
  implicit def arrayFloatEncoder[Col <: Seq[Float]]: ContextEncoder[Col]
  implicit def arrayDoubleEncoder[Col <: Seq[Double]]: ContextEncoder[Col]
  implicit def arrayDateEncoder[Col <: Seq[Date]]: ContextEncoder[Col]
  implicit def arrayLocalDateEncoder[Col <: Seq[LocalDate]]: ContextEncoder[Col]

  implicit def arrayStringDecoder[Col <: Seq[String]](implicit bf: CBF[String, Col]): ContextDecoder[Col]
  implicit def arrayBigDecimalDecoder[Col <: Seq[BigDecimal]](implicit bf: CBF[BigDecimal, Col]): ContextDecoder[Col]
  implicit def arrayBooleanDecoder[Col <: Seq[Boolean]](implicit bf: CBF[Boolean, Col]): ContextDecoder[Col]
  implicit def arrayByteDecoder[Col <: Seq[Byte]](implicit bf: CBF[Byte, Col]): ContextDecoder[Col]
  implicit def arrayShortDecoder[Col <: Seq[Short]](implicit bf: CBF[Short, Col]): ContextDecoder[Col]
  implicit def arrayIntDecoder[Col <: Seq[Int]](implicit bf: CBF[Int, Col]): ContextDecoder[Col]
  implicit def arrayLongDecoder[Col <: Seq[Long]](implicit bf: CBF[Long, Col]): ContextDecoder[Col]
  implicit def arrayFloatDecoder[Col <: Seq[Float]](implicit bf: CBF[Float, Col]): ContextDecoder[Col]
  implicit def arrayDoubleDecoder[Col <: Seq[Double]](implicit bf: CBF[Double, Col]): ContextDecoder[Col]
  implicit def arrayDateDecoder[Col <: Seq[Date]](implicit bf: CBF[Date, Col]): ContextDecoder[Col]
  implicit def arrayLocalDateDecoder[Col <: Seq[LocalDate]](implicit bf: CBF[LocalDate, Col]): ContextDecoder[Col]

  implicit def arrayMappedEncoder[I, O, Col[X] <: Seq[X]](
    implicit
    mapped: MappedEncoding[I, O],
    e:      ContextEncoder[Seq[O]]
  ): ContextEncoder[Col[I]] = {
    mappedEncoder[Col[I], Seq[O]](MappedEncoding((col: Col[I]) => col.map(mapped.f)), e)
  }

  implicit def arrayMappedDecoder[I, O, Col[X] <: Seq[X]](
    implicit
    mapped: MappedEncoding[I, O],
    d:      ContextDecoder[Seq[I]],
    bf:     Factory[O, Col[O]]
  ): ContextDecoder[Col[O]] = {
    mappedDecoder[Seq[I], Col[O]](MappedEncoding((col: Seq[I]) =>
      col.foldLeft(bf.newBuilder)((b, x) => b += mapped.f(x)).result), d)
  }
}
