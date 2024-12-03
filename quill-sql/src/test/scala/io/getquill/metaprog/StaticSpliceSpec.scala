package io.getquill.metaprog

import io.getquill.{given, _}
import io.getquill.util.prep.Mod
import io.getquill.MirrorContext.Codec.*

class StaticSpliceSpec extends MirrorSpec {
  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)
  given CompositeDecoder[Person] = deriveComposite

  "simple string splice should work" in {
    ctx.run { query[Person].filter(p => p.name == static(Mod.modVal)) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'modValValue'"
    ctx.run { query[Person].filter(p => p.age == static(Mod.modIntVal)) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.age = 123"
    ctx.run { query[Person].filter(p => p.name == static(Mod.modDef)) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'modDefValue'"
    ctx.run { query[Person].filter(p => p.name == static(Mod.modAp())) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'modApValue'"

    ctx.run { query[Person].filter(p => p.name == static(Mod.Foo.fooVal)) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'fooValValue'"
    ctx.run { query[Person].filter(p => p.name == static(Mod.Foo.fooDef)) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'fooDefValue'"
    ctx.run { query[Person].filter(p => p.name == static(Mod.Foo.fooAp())) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'fooApValue'"

    ctx.run { query[Person].filter(p => p.name == static(Mod.Foo.Bar.barVal)) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'barValValue'"
    ctx.run { query[Person].filter(p => p.name == static(Mod.Foo.Bar.barDef)) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'barDefValue'"
    ctx.run { query[Person].filter(p => p.name == static(Mod.Foo.Bar.barAp())) }.string mustEqual
      "SELECT p.name, p.age FROM Person p WHERE p.name = 'barApValue'"
  }
}
