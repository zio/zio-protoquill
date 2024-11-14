package io.getquill

import io.getquill.context.qzio.ImplicitSyntax.Implicit
import io.getquill.ZioSpec.runLayerUnsafe
import io.getquill.jdbczio.Quill

package object sqlite {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testSqliteDB"))
  object testContext extends Quill.Sqlite(Literal, pool)
}
