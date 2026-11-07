package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class BatchInsert(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def addLines(items: List[OrderLine]) = run {
    liftQuery(items).foreach(item => lines.insertValue(item))
  }
}
