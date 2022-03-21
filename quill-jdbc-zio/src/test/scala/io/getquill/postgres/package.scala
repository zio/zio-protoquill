package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Scoped
import javax.sql.DataSource

package object postgres {
  implicit val pool: Implicit[Scoped[DataSource]] = Implicit(zio.Runtime.unsafeFromLayer(DataSourceLayer.fromPrefix("testPostgresDB")))
  object testContext extends PostgresZioJdbcContext(Literal) with TestEntities
}
