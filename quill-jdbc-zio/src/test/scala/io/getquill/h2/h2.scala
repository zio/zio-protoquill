package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Managed
import javax.sql.DataSource

package object h2 {
  implicit val pool: Implicit[Managed[DataSource]] = Implicit(zio.Runtime.unsafeFromLayer(DataSourceLayer.fromPrefix("testH2DB")))
  object testContext extends H2ZioJdbcContext(Literal) with TestEntities
}
