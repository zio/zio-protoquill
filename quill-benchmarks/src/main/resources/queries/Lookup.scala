package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Lookup(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def lookup(id: CustomerId) = run(customers.filter(_.id == lift(id)))
}
