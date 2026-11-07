package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Update(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def markPaid(id: Long, expectedVersion: Int, paidAt: java.time.LocalDateTime) = run {
    openOrders.filter(o => o.id == lift(id) && o.version == lift(expectedVersion))
      .update(_.status -> lift(OrderStatus.Paid), _.paidAt -> Option(lift(paidAt)), _.version -> (lift(expectedVersion) + 1))
      .returning(order => order)
  }
}
