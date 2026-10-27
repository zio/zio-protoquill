package zio.quill

import zio.quill.context.qzio.ImplicitSyntax.Implicit
import javax.sql.DataSource

package object misc {
  implicit val pool: Implicit[DataSource] = Implicit(zio.quill.postgres.pool)
  object testContext extends PostgresZioJdbcContext(Literal) with TestEntities
}
