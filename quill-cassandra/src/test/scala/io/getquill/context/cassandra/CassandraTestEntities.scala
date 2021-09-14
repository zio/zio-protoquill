package io.getquill.context.cassandra

import io.getquill.TestEntities
import io.getquill._

trait CassandraTestEntities extends TestEntities {
  this: CassandraContext[_] =>

  case class MapFrozen(id: Map[Int, Boolean])
  inline def mapFroz = quote(query[MapFrozen])

  case class SetFrozen(id: Set[Int])
  inline def setFroz = quote(query[SetFrozen])

  case class ListFrozen(id: List[Int])
  inline def listFroz = quote(query[ListFrozen])
}
