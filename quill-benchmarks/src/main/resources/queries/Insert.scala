package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Insert(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def create(order: PurchaseOrder) = run {
    orders.insertValue(lift(order)).returningGenerated(_.id)
  }
}
