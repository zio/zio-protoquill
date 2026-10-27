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

  inline def orders: Quoted[EntityQuery[OrderTestEntity]]

  inline def insert = quote((e: OrderTestEntity) => orders.insertValue(e))
  inline def deleteAll = quote(orders.delete)
  inline def selectAll = quote(orders)
  inline def map = quote(orders.map(_.id))
  inline def filter = quote(orders.filter(_.id == 1))
  inline def withFilter = quote(orders.withFilter(_.id == 1))
  inline def sortBy = quote(orders.filter(_.id == 1).sortBy(_.i)(Ord.asc))
  inline def take = quote(orders.take(10))
  inline def entitySize = quote(orders.size)
  inline def parametrizedSize = quote { (id: Int) =>
    orders.filter(_.id == id).size
  }
  inline def distinct = quote(orders.map(_.id).distinct)
}
