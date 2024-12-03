package io.getquill

import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import MirrorContext.*
import io.getquill.MirrorContext.Codec.*

class ParticularizationSpec extends MirrorSpec {

  case class Ent(foo: String, bar: String)
  given CompositeDecoder[Ent] = deriveComposite

  "question mark particularization" - {
    val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext(PostgresDialect, Literal)
    import ctx._

    "works with query + one lift" in {
      ctx.run(query[Ent].filter(e => e.foo == lift("foo"))).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = ?", List("foo"), Static
      )
    }
    "works with query + one lift - dynamic" in {
      val q = quote(query[Ent].filter(e => e.foo == lift("foo")))
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = ?", List("foo"), Dynamic
      )
    }
    "works with query + multiple lifts" in {
      ctx.run(query[Ent].filter(e => e.foo == lift("foo") && e.bar == lift("bar"))).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = ? AND e.bar = ?", List("foo", "bar"), Static
      )
    }
    "works with query + multiple lifts - dynamic" in {
      val q = quote(query[Ent].filter(e => e.foo == lift("foo") && e.bar == lift("bar")))
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = ? AND e.bar = ?", List("foo", "bar"), Dynamic
      )
    }
    "works with list lift mixed" in {
      inline def q = quote { query[Ent].filter(e => e.bar == lift("h") && liftQuery(List("a", "b")).contains(e.foo) && e.bar == lift("t")) }
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.bar = ? AND e.foo IN (?, ?) AND e.bar = ?", List("h", "a", "b", "t"), Static
      )
    }
    "works with list lift mixed - dynamic" in {
      val q = quote { query[Ent].filter(e => e.bar == lift("h") && liftQuery(List("a", "b")).contains(e.foo) && e.bar == lift("t")) }
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.bar = ? AND e.foo IN (?, ?) AND e.bar = ?", List("h", "a", "b", "t"), Dynamic
      )
    }
  }

  "numbered mark particularization" - {
    val ctx: MirrorContext[H2Dialect, Literal] = new MirrorContext(H2Dialect, Literal)
    import ctx._

    "works with query + one lift" in {
      ctx.run(query[Ent].filter(e => e.foo == lift("foo"))).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = $1", List("foo"), Static
      )
    }
    "works with query + one lift - dynamic" in {
      val q = quote(query[Ent].filter(e => e.foo == lift("foo")))
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = $1", List("foo"), Dynamic
      )
    }
    "works with query + multiple lifts" in {
      ctx.run(query[Ent].filter(e => e.foo == lift("foo") && e.bar == lift("bar"))).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = $1 AND e.bar = $2", List("foo", "bar"), Static
      )
    }
    "works with query + multiple lifts - dynamic" in {
      val q = quote(query[Ent].filter(e => e.foo == lift("foo") && e.bar == lift("bar")))
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo = $1 AND e.bar = $2", List("foo", "bar"), Dynamic
      )
    }
    "works with explicit insert" in {
      ctx.run(query[Ent].insert(_.foo -> lift("foo"), _.bar -> lift("bar"))).triple mustBe (
        "INSERT INTO Ent (foo,bar) VALUES ($1, $2)", List("foo", "bar"), Static
      )
    }
    "works with explicit insert - dynamic" in {
      val q = quote(query[Ent].insert(_.foo -> lift("foo"), _.bar -> lift("bar")))
      ctx.run(q).triple mustBe (
        "INSERT INTO Ent (foo,bar) VALUES ($1, $2)", List("foo", "bar"), Dynamic
      )
    }
    "works with list lift" in {
      inline def q = quote { query[Ent].filter(e => liftQuery(List("a", "b")).contains(e.foo)) }
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo IN ($1, $2)", List("a", "b"), Static
      )
    }
    "works with list lift - dynamic" in {
      val q = quote { query[Ent].filter(e => liftQuery(List("a", "b")).contains(e.foo)) }
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.foo IN ($1, $2)", List("a", "b"), Dynamic
      )
    }
    "works with list lift mixed" in {
      inline def q = quote { query[Ent].filter(e => e.bar == lift("h") && liftQuery(List("a", "b")).contains(e.foo) && e.bar == lift("t")) }
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.bar = $1 AND e.foo IN ($2, $3) AND e.bar = $4", List("h", "a", "b", "t"), Static
      )
    }
    "works with list lift mixed - dynamic" in {
      val q = quote { query[Ent].filter(e => e.bar == lift("h") && liftQuery(List("a", "b")).contains(e.foo) && e.bar == lift("t")) }
      ctx.run(q).triple mustBe (
        "SELECT e.foo, e.bar FROM Ent e WHERE e.bar = $1 AND e.foo IN ($2, $3) AND e.bar = $4", List("h", "a", "b", "t"), Dynamic
      )
    }
  }
}
