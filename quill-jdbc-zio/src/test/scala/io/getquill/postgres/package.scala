package io.getquill

import io.getquill.context.qzio.ImplicitSyntax.Implicit
import io.getquill.ZioSpec.runLayerUnsafe
import io.getquill.context.AstSplicing
import io.getquill.jdbczio.Quill

package object postgres {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testPostgresDB"))
  object testContext extends Quill.Postgres(Literal, pool) with TestEntities
  object testContextSplicing extends Quill.Postgres(Literal, pool) with TestEntities with AstSplicing
}
