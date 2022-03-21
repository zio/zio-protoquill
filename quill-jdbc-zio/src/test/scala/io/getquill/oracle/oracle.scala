package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Scoped
import javax.sql.DataSource

package object oracle {
  implicit val pool: Implicit[Scoped[DataSource]] = Implicit(zio.Runtime.unsafeFromLayer(DataSourceLayer.fromPrefix("testOracleDB")))
  object testContext extends OracleZioJdbcContext(Literal) with TestEntities
}
