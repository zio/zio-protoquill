package io.getquill

import io.getquill.context.mirror.Row
import io.getquill.util.StringOps._

class FlicersSpec extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class PersonFlat(firstName: String, lastName: String, age: Int)
  case class Name(first: String, last: String)
  case class PersonNest(name: Name, age: Int)

  val s = io.getquill.MirrorSession.default

  "Filtering Standard columns" in {
    val columns = List("firstName")
    inline def q = quote {
      query[PersonFlat].filterColumns(columns)
    }
    val r = ctx.run(q)
    r.extractor(Row("firstName" -> "Joe", "lastName" -> "Bloggs", "age" -> 123), s) mustEqual
      PersonFlat("Joe","Bloggs",123)
  }

  "Filtering Nested columns" in {
    val columns = List("name")
    inline def q = quote {
      query[PersonNest].filterColumns(columns)
    }
    val r = ctx.run(q)
    r.string.collapseSpace mustEqual
      """|SELECT
         | CASE WHEN ? THEN p.first ELSE null END,
         | CASE WHEN ? THEN p.last ELSE null END,
         | CASE WHEN ? THEN p.age ELSE null END
         | FROM PersonNest p
         |""".stripMargin.collapseSpace

    r.extractor(Row("first" -> "Joe", "last" -> "Bloggs", "age" -> 123), s) mustEqual
      PersonNest(Name("Joe","Bloggs"),123)
  }
}