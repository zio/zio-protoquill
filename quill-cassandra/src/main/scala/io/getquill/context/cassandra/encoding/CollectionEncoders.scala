package io.getquill.context.cassandra.encoding

import io.getquill.context.cassandra.CassandraRowContext
import scala.jdk.CollectionConverters._
import io.getquill.generic.EncodingDsl
import java.lang.{ Boolean => JBoolean, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort, Byte => JByte }

/// TODO Is EncodingDsl needed here?
trait CollectionEncoders extends EncodingDsl with CassandraRowContext {
  this: Encoders =>

  implicit def listEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas, MapperSide.Encode]): Encoder[List[T]] =
    encoder((index, list, row, session)        => row.setList[Cas](index, list.map(row => mapper.f(row, session)).asJava))

  implicit def setEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas, MapperSide.Encode]): Encoder[Set[T]] =
    encoder((index, set, row, session) =>  row.setSet[Cas](index, set.map(row => mapper.f(row, session)).asJava))

  implicit def mapEncoder[K, V, KCas, VCas](
    implicit
    keyMapper: CassandraMapper[K, KCas, MapperSide.Encode],
    valMapper: CassandraMapper[V, VCas, MapperSide.Encode]
  ): Encoder[Map[K, V]] = encoder((index, map, row, session) => row.setMap[KCas, VCas](index, map
    .map(kv => keyMapper.f(kv._1, session) -> valMapper.f(kv._2, session)).asJava))
}
