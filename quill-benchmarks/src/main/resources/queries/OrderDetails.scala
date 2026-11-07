package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class OrderDetails(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def details(id: Long) = run {
    for {
      order <- orders.filter(_.id == lift(id))
      customer <- customers.join(_.id == order.customerId)
      shipment <- shipments.leftJoin(_.orderId == order.id)
      recipient <- customers.leftJoin(c => shipment.flatMap(_.recipientId).contains(c.id))
      payment <- payments.leftJoin(_.orderId == order.id)
      latest = states.distinctOn(_.orderId).sortBy(s => (s.orderId, s.changedAt))(Ord(Ord.asc, Ord.desc))
      state <- latest.leftJoin(_.orderId == order.id)
    } yield (order, customer, shipment, recipient, payment, state)
  }
}
