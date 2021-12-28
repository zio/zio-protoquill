package io.getquill.context.cassandra

import io.getquill.Spec
import io.getquill.context.cassandra.encoding.Encoders
import io.getquill.context.cassandra.encoding.Decoders
import io.getquill.Ord
import io.getquill._

trait QueryResultTypeCassandraSpec extends Spec {

  val context: CassandraContext[_] with Encoders with Decoders
  import context._

  case class OrderTestEntity(id: Int, i: Int)

  val entries = List(
    OrderTestEntity(1, 1),
    OrderTestEntity(2, 2),
    OrderTestEntity(3, 3)
  )

  inline def insert = quote((e: OrderTestEntity) => query[OrderTestEntity].insert(e))
  inline def deleteAll = quote(query[OrderTestEntity].delete)
  inline def selectAll = quote(query[OrderTestEntity])
  inline def map = quote(query[OrderTestEntity].map(_.id))
  inline def filter = quote(query[OrderTestEntity].filter(_.id == 1))
  inline def withFilter = quote(query[OrderTestEntity].withFilter(_.id == 1))
  inline def sortBy = quote(query[OrderTestEntity].filter(_.id == 1).sortBy(_.i)(Ord.asc))
  inline def take = quote(query[OrderTestEntity].take(10))
  inline def entitySize = quote(query[OrderTestEntity].size)
  inline def parametrizedSize = quote { (id: Int) =>
    query[OrderTestEntity].filter(_.id == id).size
  }
  inline def distinct = quote(query[OrderTestEntity].map(_.id).distinct)
}
