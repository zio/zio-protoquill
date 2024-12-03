package io.getquill.sanity

import io.getquill._

class TranslateSanityTest extends MirrorSpec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  case class Person(name: String, age: Int)

  "simple translate" in {
    val q = quote {
      query[Person].map(p => p.name)
    }
    ctx.translate(q) mustEqual
      """SELECT p.name FROM Person p"""
  }

  "translate query with one lifting" in {
    val q = quote {
      query[Person].filter(p => p.name == lift("Joe")).map(p => p.age)
    }
    ctx.translate(q) mustEqual
      """SELECT p.age FROM Person p WHERE p.name = 'Joe'"""
  }
}
