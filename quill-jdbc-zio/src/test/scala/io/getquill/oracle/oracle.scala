package io.getquill

import io.getquill.context.qzio.ImplicitSyntax.Implicit
import io.getquill.ZioSpec.runLayerUnsafe
import io.getquill.jdbczio.Quill

package object oracle {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testOracleDB"))
  object testContext extends Quill.OracleService(Literal, pool) with TestEntities
}
