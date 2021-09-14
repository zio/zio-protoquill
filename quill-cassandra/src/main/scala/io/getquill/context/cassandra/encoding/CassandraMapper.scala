package io.getquill.context.cassandra.encoding

import io.getquill.context.UdtValueLookup

/**
 * Developers API.
 *
 * End-users should rely on MappedEncoding since it's more general.
 */
case class CassandraMapper[I, O, Side <: MapperSide](f: (I, UdtValueLookup) => O)
object CassandraMapper {
  def encode[I, O](f: I => O): CassandraMapper[I, O, MapperSide.Encode] = CassandraMapper[I, O, MapperSide.Encode]((iOrig, _) => f(iOrig))
  def decode[I, O](f: I => O): CassandraMapper[I, O, MapperSide.Decode] = CassandraMapper[I, O, MapperSide.Decode]((iOrig, _) => f(iOrig))
}

sealed trait MapperSide
object MapperSide {
  sealed trait Encode extends MapperSide
  sealed trait Decode extends MapperSide
}
