package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Managed
import javax.sql.DataSource

package object mysql {
  implicit val pool: Implicit[Managed[DataSource]] = Implicit(zio.Runtime.unsafeFromLayer(DataSourceLayer.fromPrefix("testMysqlDB")))
  object testContext extends MysqlZioJdbcContext(Literal) with TestEntities
}
