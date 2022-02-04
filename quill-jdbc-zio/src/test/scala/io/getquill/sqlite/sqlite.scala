package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Managed
import javax.sql.DataSource

package object sqlite {
  implicit val pool: Implicit[Managed[DataSource]] = Implicit(zio.Runtime.unsafeFromLayer(DataSourceLayer.fromPrefix("testSqliteDB")))
  object testContext extends SqliteZioJdbcContext(Literal) with TestEntities
}
