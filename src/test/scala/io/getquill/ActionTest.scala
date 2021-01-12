package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._
import io.getquill.quoter.Quoted
import io.getquill.quoter._
import io.getquill._
import io.getquill.ast._
import io.getquill.quoter.QuotationLot
import io.getquill.quoter.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType

class ActionTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  "Entity with embedding" - {
    case class Person(name: String, age: Int)

    // Insert(Entity("Person", List()), List(Assignment(Id("x1"), Property(Id("x1"), "name"), "Joe"), Assignment(Id("x2"), Property(Id("x2"), "age"), 123)))
    "simple" in {
      val result = ctx.run(query[Person].insert(_.name -> "Joe", _.age -> 123))
      result.string mustEqual "INSERT INTO Person (name,age) VALUES ('Joe', 123)"
      result.executionType mustEqual ExecutionType.Static
      result.prepareRow.data.toList mustEqual List() //hello
    } //hello

    // Doesn't work
    "simple - compiletime" in {
      val q = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) }
      val result = ctx.run(q)
      result.string mustEqual "INSERT INTO Person (name,age) VALUES ('Joe', 123)"
      result.executionType mustEqual ExecutionType.Dynamic
      result.prepareRow.data.toList mustEqual List()
    }

    "macro" in {
      val result = ctx.run(query[Person].insert(Person("Joe", 123)))
      result.string mustEqual "INSERT INTO Person (name,age) VALUES ('Joe', 123)"
      result.executionType mustEqual ExecutionType.Static
      result.prepareRow.data.toList mustEqual List()
    }

    "macro with lift" in {
      val result = ctx.run(query[Person].insert(Person(lift("Joe"), 123)))
      result.string mustEqual "INSERT INTO Person (name,age) VALUES (?, 123)"
      result.executionType mustEqual ExecutionType.Static
      result.prepareRow.data.toList mustEqual List("Joe")
    }
  }

  // TODO Need more testing of this for multiple use-cases
//   "Entity with embedding" - {
//     case class Address(street:String, zip:Int) extends Embedded
//     case class Person(name: String, age: Int, address: Address)
//     inline def people = quote { query[Person] }
//     def peopleRuntime = quote { query[Person] }
//   }
} 