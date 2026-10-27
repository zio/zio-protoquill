package zio.quill.context.cassandra

import zio.quill.TestEntities
import zio.quill._

trait CassandraTestEntities extends TestEntities {
  this: CassandraContext[_] =>

  case class MapFrozen(id: Map[Int, Boolean])
  inline def mapFroz = quote(query[MapFrozen])

  case class SetFrozen(id: Set[Int])
  inline def setFroz = quote(query[SetFrozen])

  case class ListFrozen(id: List[Int])
  inline def listFroz = quote(query[ListFrozen])
}
