package io.getquill.context.cassandra

import com.datastax.oss.driver.api.core.cql.Row

import java.util.{Date, UUID}
import io.getquill.{MappedEncoding, NamingStrategy, ProductDecoders}
import io.getquill.context.{Context, UdtValueLookup}
import io.getquill.context.cassandra.encoding.{CassandraMapper, CassandraTypes, Decoders, Encoders, MapperSide}
import io.getquill.generic.GenericNullChecker

import java.time.{Instant, LocalDate, ZoneId, ZonedDateTime}
import scala.reflect.ClassTag

/*
override type PrepareRow = BoundStatement
  override type ResultRow = Row
 */

trait CassandraCodecsBase[Session]
  extends ProductDecoders
  with Encoders
  with Decoders
  with CassandraTypes {

  // TODO some kind of global-configuration of timezone? Implicits? System property?
  protected val zoneId = ZoneId.systemDefault

  implicit val encodeJava8ZonedDateTime: MappedEncoding[ZonedDateTime, Instant] = MappedEncoding(zdt => zdt.toInstant)
  implicit val decodeJava8ZonedDateTime: MappedEncoding[Instant, ZonedDateTime] = MappedEncoding(d => ZonedDateTime.ofInstant(d, zoneId))

  override type NullChecker = CassandraNullChecker
  class CassandraNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: Row): Boolean = row.isNull(index)
  }
  implicit val nullChecker: NullChecker = new CassandraNullChecker()
}


trait CassandraContext[+N <: NamingStrategy]
  extends Context[CqlIdiom, N]
  //with Encodings
  //with UdtMetaDsl
  // with Ops
  {
//
//  implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]]
//  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]]
//
//  implicit val stringDecoder: Decoder[String]
//  implicit val bigDecimalDecoder: Decoder[BigDecimal]
//  implicit val booleanDecoder: Decoder[Boolean]
//  implicit val byteDecoder: Decoder[Byte]
//  implicit val shortDecoder: Decoder[Short]
//  implicit val intDecoder: Decoder[Int]
//  implicit val longDecoder: Decoder[Long]
//  implicit val floatDecoder: Decoder[Float]
//  implicit val doubleDecoder: Decoder[Double]
//  implicit val byteArrayDecoder: Decoder[Array[Byte]]
//  implicit val uuidDecoder: Decoder[UUID]
//  implicit val timestampDecoder: Decoder[Instant]
//  implicit val cassandraLocalDateDecoder: Decoder[LocalDate]
//
//  implicit val stringEncoder: Encoder[String]
//  implicit val bigDecimalEncoder: Encoder[BigDecimal]
//  implicit val booleanEncoder: Encoder[Boolean]
//  implicit val byteEncoder: Encoder[Byte]
//  implicit val shortEncoder: Encoder[Short]
//  implicit val intEncoder: Encoder[Int]
//  implicit val longEncoder: Encoder[Long]
//  implicit val floatEncoder: Encoder[Float]
//  implicit val doubleEncoder: Encoder[Double]
//  implicit val byteArrayEncoder: Encoder[Array[Byte]]
//  implicit val uuidEncoder: Encoder[UUID]
//  implicit val timestampEncoder: Encoder[Instant]
//  implicit val cassandraLocalDateEncoder: Encoder[LocalDate]
//
//  implicit def listDecoder[T, Cas](implicit mapper: CassandraMapper[Cas, T, MapperSide.Decode], ct: ClassTag[Cas]): Decoder[List[T]]
//  implicit def setDecoder[T, Cas](implicit mapper: CassandraMapper[Cas, T, MapperSide.Decode], ct: ClassTag[Cas]): Decoder[Set[T]]
//  implicit def mapDecoder[K, V, KCas, VCas](
//    implicit
//    keyMapper: CassandraMapper[KCas, K, MapperSide.Decode],
//    valMapper: CassandraMapper[VCas, V, MapperSide.Decode],
//    a: ClassTag[KCas],
//    b: ClassTag[VCas]
//  ): Decoder[Map[K, V]]
//
//  implicit def listEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas, MapperSide.Encode], ct: ClassTag[Cas]): Encoder[List[T]]
//  implicit def setEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas, MapperSide.Encode], ct: ClassTag[Cas]): Encoder[Set[T]]
//  implicit def mapEncoder[K, V, KCas, VCas](
//    implicit
//    keyMapper: CassandraMapper[K, KCas, MapperSide.Encode],
//    valMapper: CassandraMapper[V, VCas, MapperSide.Encode],
//    a: ClassTag[KCas],
//    b: ClassTag[VCas]
//  ): Encoder[Map[K, V]]
}