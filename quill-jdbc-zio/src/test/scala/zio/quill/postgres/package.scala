package zio.quill

import zio.quill.context.qzio.ImplicitSyntax.Implicit
import zio.quill.ZioSpec.runLayerUnsafe
import zio.quill.context.AstSplicing
import zio.quill.jdbczio.Quill

package object postgres {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testPostgresDB"))
  object testContext extends Quill.Postgres(Literal, pool) with TestEntities
  object testContextSplicing extends Quill.Postgres(Literal, pool) with TestEntities with AstSplicing
}
