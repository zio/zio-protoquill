package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Exists(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def awaitingShipment(customerId: CustomerId) = {
    inline def readyToShip(inline order: PurchaseOrder) = quote {
      lines.filter(l => l.orderId == order.id && l.quantity > 0).nonEmpty &&
        shipments.filter(_.orderId == order.id).isEmpty
    }
    run(openOrders.filter(o => o.customerId == lift(customerId) && readyToShip(o)))
  }
}
