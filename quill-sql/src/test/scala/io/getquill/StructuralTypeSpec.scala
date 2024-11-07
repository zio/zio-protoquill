package io.getquill

import io.getquill.context.ExecutionType
import reflect.Selectable.reflectiveSelectable

class StructuralTypeSpec extends Spec {

  case class Person(name: String, age: Int)
  given MirrorContext.GenericDecoder[Person] = MirrorContext.deriveDecoder

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  "quoted function" in {
    inline def filterByName[T <: { def name: String }] = quote {
      (q: Query[T]) => q.filter(p => p.name == "Joe")
    }

    val mirror = ctx.run(filterByName(query[Person]))
    mirror.triple mustEqual (
      (
        "SELECT p.name, p.age FROM Person p WHERE p.name = 'Joe'",
        List(),
        ExecutionType.Static
      )
    )
  }

  "inline function" in {
    inline def filterByName[T <: { def name: String }](inline q: Query[T]) =
      q.filter(p => p.name == "Joe")

    val mirror = ctx.run(filterByName(query[Person]))
    mirror.triple mustEqual (
      (
        "SELECT p.name, p.age FROM Person p WHERE p.name = 'Joe'",
        List(),
        ExecutionType.Static
      )
    )
  }

  "quoted function dynamic" in {
    def filterByName[T <: { def name: String }] = quote {
      (q: Query[T]) => q.filter(p => p.name == "Joe")
    }

    val mirror = ctx.run(filterByName(query[Person]))
    mirror.triple mustEqual (
      (
        "SELECT p.name, p.age FROM Person p WHERE p.name = 'Joe'",
        List(),
        ExecutionType.Dynamic
      )
    )
  }
}
