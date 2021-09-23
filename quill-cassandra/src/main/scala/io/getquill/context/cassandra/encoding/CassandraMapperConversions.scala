package io.getquill.context.cassandra.encoding

import io.getquill.MappedEncoding

trait CassandraMapperConversions extends CassandraMapperConversionsLowPriorityImplicits {

  implicit def cassandraIdentityEncodeMapper[Cas](implicit cas: CassandraType[Cas]): CassandraMapper[Cas, Cas, MapperSide.Encode] =
    CassandraMapper((i, _) => i)

  implicit def cassandraIdentityDecodeMapper[Cas](implicit cas: CassandraType[Cas]): CassandraMapper[Cas, Cas, MapperSide.Decode] =
    CassandraMapper((i, _) => i)

  implicit def cassandraMapperEncode[T, Cas](
    implicit
    m:   MappedEncoding[T, Cas],
    cas: CassandraType[Cas]
  ): CassandraMapper[T, Cas, MapperSide.Encode] = CassandraMapper((i, _) => m.f(i))

  implicit def cassandraMapperDecode[T, Cas](
    implicit
    m:   MappedEncoding[Cas, T],
    cas: CassandraType[Cas]
  ): CassandraMapper[Cas, T, MapperSide.Decode] = CassandraMapper((i, _) => m.f(i))
}

trait CassandraMapperConversionsLowPriorityImplicits {

  implicit def cassandraMapperEncodeRec[I, O, Cas](
    implicit
    me: MappedEncoding[I, O],
    cm: CassandraMapper[O, Cas, MapperSide.Encode]
  ): CassandraMapper[I, Cas, MapperSide.Encode] = CassandraMapper((i, lookup) => cm.f(me.f(i), lookup))

  implicit def cassandraMapperDecodeRec[I, O, Cas](
    implicit
    m:  MappedEncoding[I, O],
    cm: CassandraMapper[Cas, I, MapperSide.Decode]
  ): CassandraMapper[Cas, O, MapperSide.Decode] = CassandraMapper((i, lookup) => m.f(cm.f(i, lookup)))
}
