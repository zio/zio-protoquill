package io.getquill.metaprog

import io.getquill._
import io.getquill.util.prep.Mod

object StaticSpliceSpec extends Spec {
  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  "simple string splice should work" in {
    val q = static(Mod.Foo.Bar.barVal)
  }
}