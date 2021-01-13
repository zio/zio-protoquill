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
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic

class ActionTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  // TODO, next up we need to have this: query[Person].insert(lift(Person("Joe", 123))), that's a case class lift

  "insert for simple entity should work for" - {
    // Insert(Entity("Person", List()), List(Assignment(Id("x1"), Property(Id("x1"), "name"), "Joe"), Assignment(Id("x2"), Property(Id("x2"), "age"), 123)))
    "simple, inline query" - {
      inline def a = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) } // Insert "assignment form"
      inline def q = quote { query[Person].insert(Person("Joe", 123)) }            // Insert entity form
      "regular" in {
        ctx.run(q).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Static)
        ctx.run(a).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Static)
      }
      "simple with schemaMeta" in {
        inline given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")
        ctx.run(q).triple mustEqual ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Static)
        ctx.run(a).triple mustEqual ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Static)
      }
      // TODO Doing this with a runtime query should throw an exception (for now)
      "simple with insert meta" in {
        inline given personMeta: InsertMeta[Person] = insertMeta[Person](_.age)
        ctx.run(q).triple mustEqual ("INSERT INTO Person (name) VALUES ('Joe')", List(), Static)
        ctx.run(a).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Static)
      }
      // TODO Doing this with a runtime query should throw an exception (for now)
      "simple with schemaMeta and insert meta" in {
        inline given personMeta: InsertMeta[Person] = insertMeta[Person](_.age)
        inline given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")
        ctx.run(q).triple mustEqual ("INSERT INTO tblPerson (colName) VALUES ('Joe')", List(), Static)
        ctx.run(a).triple mustEqual ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Static)
      }
      // TODO Doing this with a runtime query should throw an exception (for now)
      "simple with schemaMeta with extra columns and insert meta" in {
        inline given personSchema: InsertMeta[Person] = insertMeta[Person](_.age)
        inline given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName", _.age -> "colAge")
        ctx.run(q).triple mustEqual ("INSERT INTO tblPerson (colName) VALUES ('Joe')", List(), Static)
        ctx.run(a).triple mustEqual ("INSERT INTO tblPerson (colName,colAge) VALUES ('Joe', 123)", List(), Static)
      }
    }

    "simple - runtime" in {
      val a = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) }
      val q = quote { query[Person].insert(Person("Joe", 123)) }
      ctx.run(a).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Dynamic)
      ctx.run(q).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Dynamic)
    }

    "direct" in {
      ctx.run(query[Person].insert(_.name -> "Joe", _.age -> 123)).triple mustEqual
        ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Static)
    }

    "auto-quote" in {
      val result = ctx.run(query[Person].insert(Person("Joe", 123)))
      result.string mustEqual "INSERT INTO Person (name,age) VALUES ('Joe', 123)"
      result.executionType mustEqual ExecutionType.Static
      result.prepareRow.data.toList mustEqual List()
    }

    "auto-quote with lift" in {
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