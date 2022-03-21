package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Scoped
import javax.sql.DataSource

package object h2 {
  implicit val pool: Implicit[Scoped[DataSource]] = Implicit(zio.Runtime.unsafeFromLayer(DataSourceLayer.fromPrefix("testH2DB")))
  object testContext extends H2ZioJdbcContext(Literal) with TestEntities
}
