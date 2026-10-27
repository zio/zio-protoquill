package zio.quill

import zio.quill.context.qzio.ImplicitSyntax.Implicit
import zio.quill.ZioSpec.runLayerUnsafe
import zio.quill.jdbczio.Quill

package object oracle {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testOracleDB"))
  object testContext extends Quill.Oracle(Literal, pool) with TestEntities
}
