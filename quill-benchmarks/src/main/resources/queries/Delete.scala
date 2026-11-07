package quill.bench.queries

import zio.quill.*
import zio.quill.extras.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Delete(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def removeUnpaidDrafts(before: java.time.LocalDateTime) = run {
    orders.filter(o => o.status == lift(OrderStatus.Draft) && o.paidAt.isEmpty && o.placedAt < lift(before))
      .filter(o => lines.filter(_.orderId == o.id).isEmpty)
      .filter(o => shipments.filter(_.orderId == o.id).isEmpty)
      .delete
  }
}
