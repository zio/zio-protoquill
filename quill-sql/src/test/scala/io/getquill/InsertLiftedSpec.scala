package io.getquill

import io.getquill.context.mirror.Row
import io.getquill.context.ExecutionType

class InsertLiftedSpec extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal) // //
  import ctx._

  "Lifted Insert should work for" - {
    "Persion/Name/First/value:String`" in {
      case class First(value: String)
      case class Name(first: First, last: String)
      case class Person(name: Name, age: Int)
      given MirrorContext.CompositeDecoder[Person] = MirrorContext.deriveComposite
      given MirrorContext.CompositeDecoder[Name] = MirrorContext.deriveComposite
      given MirrorContext.CompositeDecoder[First] = MirrorContext.deriveComposite

      inline def q = quote { query[Person].insertValue(lift(Person(Name(First("Joe"), "Bloggs"), 123))) }
      ctx.run(q).triple mustEqual (
        "INSERT INTO Person (value,last,age) VALUES (?, ?, ?)",
        List("Joe", "Bloggs", 123),
        ExecutionType.Static
      )
    }

    "Persion/Option[Name]/First/value:Option[String]`" in {
      case class First(value: Option[String])
      case class Name(first: First, last: String)
      case class Person(name: Option[Name], age: Int)

      inline def q = quote { query[Person].insertValue(lift(Person(Some(Name(First(Some("Joe")), "Bloggs")), 123))) }
      ctx.run(q).triple mustEqual (
        "INSERT INTO Person (value,last,age) VALUES (?, ?, ?)",
        List(Some("Joe"), Some("Bloggs"), 123),
        ExecutionType.Static
      )
    }

    "Persion/Option[Name]/First/value:String`" in {
      case class First(value: String)
      case class Name(first: First, last: String)
      case class Person(name: Option[Name], age: Int)

      inline def q = quote { query[Person].insertValue(lift(Person(Some(Name(First("Joe"), "Bloggs")), 123))) }
      ctx.run(q).triple mustEqual (
        "INSERT INTO Person (value,last,age) VALUES (?, ?, ?)",
        List(Some("Joe"), Some("Bloggs"), 123),
        ExecutionType.Static
      )
    }

    "Persion/Option[Name]/First/Value/value:Option[String]`" in {
      case class Value(value: Option[String])
      case class First(value: Value)
      case class Name(first: First, last: String)
      case class Person(name: Option[Name], age: Int)

      inline def q = quote { query[Person].insertValue(lift(Person(Some(Name(First(Value(Some("Joe"))), "Bloggs")), 123))) }
      ctx.run(q).triple mustEqual (
        "INSERT INTO Person (value,last,age) VALUES (?, ?, ?)",
        List(Some("Joe"), Some("Bloggs"), 123),
        ExecutionType.Static
      )
    }
  }
}
