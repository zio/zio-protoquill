package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Search(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def search(ids: List[CustomerId], minimum: BigDecimal, note: Option[String], offset: Int, size: Int) = run {
    openOrders
      .filter(o => liftQuery(ids).contains(o.customerId) && o.total >= lift(minimum))
      .filter(o => lift(note).forall(n => o.note.exists(_.like(n))))
      .sortBy(o => (o.placedAt, o.id))(Ord(Ord.desc, Ord.desc))
      .drop(lift(offset)).take(lift(size))
  }
}
