package io.getquill.context

import io.getquill.Spec
import io.getquill._

class Scala3FeaturesSpec extends Spec {
  val ctx = new SqlMirrorContext(PostgresDialect, Literal)
  import ctx._

  "Scala 3 features should work with query expansion" - {
    case class Person(name: String, age: Int)

    "inline if" in {
      inline def filterPerson(inline q: Query[Person], inline doFilter: Boolean) =
        inline if (doFilter)
          q.filter(p => p.name == "Joe")
        else
          q

      ctx.run(filterPerson(query[Person], true)).string mustEqual
        "SELECT p.name, p.age FROM Person p WHERE p.name = 'Joe'"

      ctx.run(filterPerson(query[Person], false)).string mustEqual
        "SELECT x.name, x.age FROM Person x"
    }

    "inline case class match" - {

      sealed trait Filter
      object Filter:
        case class ByName(name: String)      extends Filter
        case class ByAge(from: Int, to: Int) extends Filter

      enum FilterEnum:
        case ByName(name: String)      extends FilterEnum
        case ByAge(from: Int, to: Int) extends FilterEnum

      // Can't do it like this:
      /*
      "with lift" in {
        inline def filterPerson(inline q: Query[Person])(inline f: Filter) =
          inline f match
            case Filter.ByName(nn) => q.filter(p => p.name == lift(nn))
            case Filter.ByAge(from, to) => q.filter(p => p.age > from && p.age < to)

        ctx.run(filterPerson(query[Person])(Filter.ByName("Joe")))
      }
       */

      // Need to do it like this
      "with lift" in {
        inline def filterPerson(inline q: Query[Person])(inline f: Filter) =
          inline f match
            case Filter.ByName(name)    => q.filter(p => p.name == name)
            case Filter.ByAge(from, to) => q.filter(p => p.age > from && p.age < to)

        ctx.run(filterPerson(query[Person])(Filter.ByName(lift("Joe")))).triple mustEqual (
          "SELECT p.name, p.age FROM Person p WHERE p.name = ?",
          List("Joe"),
          ExecutionType.Static
        )
      }

      // Crazy issue that I need to file
      // "with lift - enum" in {
      //   inline def filterPerson(inline q: Query[Person])(inline f: FilterEnum) =
      //     inline f match
      //       case FilterEnum.ByName(name) => q.filter(p => p.name == name)
      //       case FilterEnum.ByAge(from, to) => q.filter(p => p.age > from && p.age < to)

      //   ctx.run(filterPerson(query[Person])(FilterEnum.ByName(lift("Joe")))).triple mustEqual (
      //     "SELECT p.name, p.age FROM Person p WHERE p.name = ?",
      //     List("Joe"),
      //     ExecutionType.Static
      //   )
      // }

      "regular" in {
        inline def filterPerson(inline q: Query[Person])(inline f: Filter) =
          inline f match
            case Filter.ByName(name)    => q.filter(p => p.name == name)
            case Filter.ByAge(from, to) => q.filter(p => p.age > from && p.age < to)

        ctx.run(filterPerson(query[Person])(Filter.ByName("Joe"))).triple mustEqual (
          "SELECT p.name, p.age FROM Person p WHERE p.name = 'Joe'",
          List(),
          ExecutionType.Static
        )
        ctx.run(filterPerson(query[Person])(Filter.ByAge(22, 33))).triple mustEqual (
          "SELECT p.name, p.age FROM Person p WHERE p.age > 22 AND p.age < 33",
          List(),
          ExecutionType.Static
        )
      }
    }
  }
}
