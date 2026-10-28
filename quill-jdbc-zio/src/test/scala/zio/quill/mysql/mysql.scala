package zio.quill

import zio.quill.context.qzio.ImplicitSyntax.Implicit
import zio.quill.ZioSpec.runLayerUnsafe
import zio.quill.jdbczio.Quill

package object mysql {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testMysqlDB"))
  object testContext extends Quill.Mysql(Literal, pool) with TestEntities
}
