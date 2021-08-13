package io.getquill.metaprog

import io.getquill._
import io.getquill.util.prep.Mod
import io.getquill.util.prep.Constants

class StaticSpliceSpec extends Spec {
  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  "simple string splice should work" in { //hello

    ctx.run { query[Person].filter(p => p.name == static(Constants.Joe)) }
    ctx.run { query[Person].filter(p => p.name == static(Mod.modVal)) }
    ctx.run { query[Person].filter(p => p.name == static(Mod.modDef)) }
    ctx.run { query[Person].filter(p => p.name == static(Mod.Foo.Bar.barVal)) }
    ctx.run { query[Person].filter(p => p.age == static(Mod.modIntVal)) }
  }
}