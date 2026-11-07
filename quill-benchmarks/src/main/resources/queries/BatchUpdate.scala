package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class BatchUpdate(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def adjustStock(changes: List[Stock]) = run {
    liftQuery(changes).foreach(change =>
      stock.filter(s => s.productId == change.productId && s.warehouseId == change.warehouseId)
        .update(s => s.quantity -> (s.quantity + change.quantity), _.updatedAt -> change.updatedAt)
    )
  }
}
