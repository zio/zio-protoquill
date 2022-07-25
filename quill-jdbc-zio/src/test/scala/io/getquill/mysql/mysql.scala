package io.getquill

import io.getquill.context.qzio.ImplicitSyntax.Implicit
import io.getquill.ZioSpec.runLayerUnsafe
import io.getquill.jdbczio.Quill

package object mysql {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testMysqlDB"))
  object testContext extends Quill.MysqlService(Literal, pool) with TestEntities
}
