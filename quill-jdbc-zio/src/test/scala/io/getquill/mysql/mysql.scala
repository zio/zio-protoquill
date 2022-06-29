package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Scoped
import javax.sql.DataSource

package object mysql {
  implicit val pool: Implicit[Scoped[DataSource]] = zio.Unsafe.unsafe { Implicit(zio.Runtime.unsafe.fromLayer(DataSourceLayer.fromPrefix("testMysqlDB"))) }
  object testContext extends MysqlZioJdbcContext(Literal) with TestEntities
}
