package zio.quill.context.cassandra

import zio.quill.Spec
import zio.quill.context.mirror.Row
import zio.quill.context.cassandra.mirrorContext._
import zio.quill._

class BindVariablesSpec extends Spec {

  "binds lifted values" in {
    inline def q(inline i: Int) =
      quote {
        query[TestEntity].filter(e => e.i == lift(i))
      }
    val mirror = mirrorContext.run(q(2))
    mirror.string mustEqual "SELECT s, i, l, o, b FROM TestEntity WHERE i = ?"
    mirror.prepareRow mustEqual Row(2)
  }
}
