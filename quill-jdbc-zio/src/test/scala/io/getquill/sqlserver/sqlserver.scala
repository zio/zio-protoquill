package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Scoped
import javax.sql.DataSource

package object sqlserver {
  implicit val pool: Implicit[Scoped[DataSource]] = zio.Unsafe.unsafe { Implicit(zio.Runtime.unsafe.fromLayer(DataSourceLayer.fromPrefix("testSqlServerDB"))) }
  object testContext extends SqlServerZioJdbcContext(Literal) with TestEntities
}
