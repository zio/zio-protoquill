package io.getquill.context.cassandra.encoding

import io.getquill.context.cassandra.CassandraRowContext
import io.getquill.context.cassandra.util.ClassTagConversions.asClassOf

import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import io.getquill.generic.EncodingDsl

/// TODO Is EncodingDsl needed here?
trait CollectionDecoders extends EncodingDsl with CassandraRowContext {
  this: Decoders =>

  // TODO Remove variable b and put directly
  implicit def listDecoder[T, Cas](implicit mapper: CassandraMapper[Cas, T, MapperSide.Decode], ct: ClassTag[Cas]): Decoder[List[T]] = {
    val b: BaseDecoder[List[T]] = (index, row, session) => row.getList[Cas](index, asClassOf[Cas]).asScala.map(row => mapper.f(row, session)).toList
    decoder(b)
  }

  implicit def setDecoder[T, Cas](implicit mapper: CassandraMapper[Cas, T, MapperSide.Decode], ct: ClassTag[Cas]): Decoder[Set[T]] = {
    val b: BaseDecoder[Set[T]] = (index, row, session) => row.getSet[Cas](index, asClassOf[Cas]).asScala.map(row => mapper.f(row, session)).toSet
    decoder(b)
  }

  implicit def mapDecoder[K, V, KCas, VCas](
    implicit
    keyMapper: CassandraMapper[KCas, K, MapperSide.Decode],
    valMapper: CassandraMapper[VCas, V, MapperSide.Decode],
    a: ClassTag[KCas],
    b: ClassTag[VCas]
  ): Decoder[Map[K, V]] =
    decoder((index, row, session) =>
      row.getMap[KCas, VCas](index, asClassOf[KCas], asClassOf[VCas])
        .asScala.map(kv => keyMapper.f(kv._1, session) -> valMapper.f(kv._2, session)).toMap
    )
}
