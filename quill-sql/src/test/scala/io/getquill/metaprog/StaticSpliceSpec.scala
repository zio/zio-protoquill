package io.getquill.metaprog

import io.getquill._
import io.getquill.util.prep.Mod

class StaticSpliceSpec extends Spec {
  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  "simple string splice should work" in {
    inline def q = quote { static(Mod.Foo.Bar.barVal) } //helloooooo
    println(io.getquill.util.Messages.qprint(q.ast))

    inline def q1 = quote { query[Person].filter(p => p.name == static(Mod.Foo.Bar.barVal)) }
    println(ctx.run(q1))
  }
}