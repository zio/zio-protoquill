package io.getquill

import java.util.Date
import io.getquill.context.cassandra.encoding.{CassandraMapper, CassandraType, MapperSide}
import io.getquill.context.cassandra.{CassandraContext, CqlIdiom}

import java.time.{Instant, LocalDate}
import scala.reflect.ClassTag

class CassandraMirrorContextWithQueryProbing extends CassandraMirrorContext(Literal) with QueryProbing

class CassandraMirrorContext[+Naming <: NamingStrategy](naming: Naming)
    extends MirrorContext[CqlIdiom, Naming](CqlIdiom, naming)
    with CassandraContext[Naming] {

  implicit val timestampDecoder: Decoder[Instant]            = decoder[Instant]
  implicit val timestampEncoder: Encoder[Instant]            = encoder[Instant]
  implicit val cassandraLocalDateDecoder: Decoder[LocalDate] = decoder[LocalDate]
  implicit val cassandraLocalDateEncoder: Encoder[LocalDate] = encoder[LocalDate]

  implicit def listDecoder[T, Cas](implicit
    mapper: CassandraMapper[Cas, T, MapperSide.Decode],
    ct: ClassTag[Cas]
  ): Decoder[List[T]] = decoderUnsafe[List[T]]
  implicit def setDecoder[T, Cas](implicit
    mapper: CassandraMapper[Cas, T, MapperSide.Decode],
    ct: ClassTag[Cas]
  ): Decoder[Set[T]] = decoderUnsafe[Set[T]]
  implicit def mapDecoder[K, V, KCas, VCas](implicit
    keyMapper: CassandraMapper[KCas, K, MapperSide.Decode],
    valMapper: CassandraMapper[VCas, V, MapperSide.Decode],
    a: ClassTag[KCas],
    b: ClassTag[VCas]
  ): Decoder[Map[K, V]] = decoderUnsafe[Map[K, V]]

  implicit def listEncoder[T, Cas](implicit
    mapper: CassandraMapper[T, Cas, MapperSide.Encode],
    ct: ClassTag[Cas]
  ): Encoder[List[T]] = encoder[List[T]]
  implicit def setEncoder[T, Cas](implicit
    mapper: CassandraMapper[T, Cas, MapperSide.Encode],
    ct: ClassTag[Cas]
  ): Encoder[Set[T]] = encoder[Set[T]]
  implicit def mapEncoder[K, V, KCas, VCas](implicit
    keyMapper: CassandraMapper[K, KCas, MapperSide.Encode],
    valMapper: CassandraMapper[V, VCas, MapperSide.Encode],
    a: ClassTag[KCas],
    b: ClassTag[VCas]
  ): Encoder[Map[K, V]] = encoder[Map[K, V]]

  implicit def udtCassandraType[T <: Udt]: CassandraType[T] = CassandraType.of[T]
  implicit def udtDecoder[T <: Udt: ClassTag]: Decoder[T]   = decoder[T]
  implicit def udtEncoder[T <: Udt]: Encoder[T]             = encoder[T]
}
