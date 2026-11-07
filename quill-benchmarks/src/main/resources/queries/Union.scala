package quill.bench.queries

import zio.quill.*
import zio.quill.extras.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Union(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def customerCountries(since: java.time.LocalDateTime) = run {
    customers.filter(_.active).map(c => Country(c.country))
      .unionAll(orders.filter(_.placedAt >= lift(since)).map(o => Country(o.shipCountry)))
      .distinct.sortBy(_.code)
  }
}
