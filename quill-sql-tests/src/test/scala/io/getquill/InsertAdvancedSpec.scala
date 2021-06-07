package io.getquill

import scala.language.implicitConversions
import io.getquill.Quoted

import io.getquill.ast._
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic

class InsertAdvancedSpec extends Spec with Inside {
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
      "simple with insert meta" in {
        inline given personMeta: InsertMeta[Person] = insertMeta[Person](_.age)
        ctx.run(q).triple mustEqual ("INSERT INTO Person (name) VALUES ('Joe')", List(), Static)
        ctx.run(a).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Static)
      }
      "simple with insert meta - compact" in {
        inline given InsertMeta[Person] = insertMeta(_.age)
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
      result.info.executionType mustEqual ExecutionType.Static
      result.prepareRow.data.toList mustEqual List()
    }

    "auto-quote with lift" in {
      val result = ctx.run(query[Person].insert(Person(lift("Joe"), 123)))
      result.triple mustEqual (
        "INSERT INTO Person (name,age) VALUES (?, 123)",
        List("Joe"),
        ExecutionType.Static
      )
    }
  }

  "update for simple entity should work for" - {
    // Insert(Entity("Person", List()), List(Assignment(Id("x1"), Property(Id("x1"), "name"), "Joe"), Assignment(Id("x2"), Property(Id("x2"), "age"), 123)))
    "simple, inline query" - {
      inline def a = quote { query[Person].update(_.name -> "Joe", _.age -> 123) } // Insert "assignment form"
      inline def q = quote { query[Person].update(Person("Joe", 123)) }            // Insert entity form
      "regular" in {
        ctx.run(q).triple mustEqual ("UPDATE Person SET name = 'Joe', age = 123", List(), Static)
        ctx.run(a).triple mustEqual ("UPDATE Person SET name = 'Joe', age = 123", List(), Static)
      }
      "simple with schemaMeta" in {
        inline given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")
        ctx.run(q).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Static)
        ctx.run(a).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Static)
      }
      // TODO Doing this with a runtime query should throw an exception (for now)
      "simple with update meta" in {
        inline given personMeta: UpdateMeta[Person] = updateMeta[Person](_.age)
        ctx.run(q).triple mustEqual ("UPDATE Person SET name = 'Joe'", List(), Static)
        ctx.run(a).triple mustEqual ("UPDATE Person SET name = 'Joe', age = 123", List(), Static)
      }
      // TODO Doing this with a runtime query should throw an exception (for now)
      "simple with schemaMeta and update meta" in {
        inline given personMeta: UpdateMeta[Person] = updateMeta[Person](_.age)
        inline given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")
        ctx.run(q).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe'", List(), Static)
        ctx.run(a).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Static)
      }
      // TODO Doing this with a runtime query should throw an exception (for now)
      "simple with schemaMeta with extra columns and update meta" in {
        inline given personSchema: UpdateMeta[Person] = updateMeta[Person](_.age)
        inline given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName", _.age -> "colAge")
        ctx.run(q).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe'", List(), Static)
        ctx.run(a).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe', colAge = 123", List(), Static)
      }
    }

    "simple - runtime" in {
      val a = quote { query[Person].update(_.name -> "Joe", _.age -> 123) }
      val q = quote { query[Person].update(Person("Joe", 123)) }
      ctx.run(a).triple mustEqual ("UPDATE Person SET name = 'Joe', age = 123", List(), Dynamic)
      ctx.run(q).triple mustEqual ("UPDATE Person SET name = 'Joe', age = 123", List(), Dynamic)
    }

    "direct" in {
      ctx.run(query[Person].update(_.name -> "Joe", _.age -> 123)).triple mustEqual
        ("UPDATE Person SET name = 'Joe', age = 123", List(), Static)
    }

    "auto-quote" in {
      val result = ctx.run(query[Person].update(Person("Joe", 123)))
      result.string mustEqual "UPDATE Person SET name = 'Joe', age = 123"
      result.info.executionType mustEqual ExecutionType.Static
      result.prepareRow.data.toList mustEqual List()
    }

    "auto-quote with lift" in {
      val result = ctx.run(query[Person].update(Person(lift("Joe"), 123)))
      result.triple mustEqual (
        "UPDATE Person SET name = ?, age = 123",
        List("Joe"),
        ExecutionType.Static
      )
    }
  }

  // Variation of this with only InsertMeta, and well as both InsertMeta and SchemaMeta (inline and dynamic)
  "entity insert with dynamic components should work for" - {
    "given queries in an outer scope" - {
      inline def a = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) } // Insert "assignment form"
      inline def q = quote { query[Person].insert(Person("Joe", 123)) }            // Insert entity form
      val adyn = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) } // Dynamic Insert "assignment form"
      val qdyn = quote { query[Person].insert(Person("Joe", 123)) }            // Dynamic Insert entity form

      "dynamic schema makes whole query dynamic - it will plug into runtime queries post-facto" in {
        given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")

        // For static queries `insert` macro is only being evaluated right here so `given sm` will change names
        ctx.run(q).triple mustEqual    ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Dynamic)
        ctx.run(a).triple mustEqual    ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Dynamic)

        // For dynamic queries `insert` macro is already evaluated therefore `given sm` will not change the column names
        ctx.run(qdyn).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Dynamic)
        ctx.run(adyn).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Dynamic)
      }
    }
  }

  "(update) entity insert with dynamic components should work for" - {
    "given queries in an outer scope" - {
      inline def a = quote { query[Person].update(_.name -> "Joe", _.age -> 123) } // Insert "assignment form"
      inline def q = quote { query[Person].update(Person("Joe", 123)) }            // Insert entity form
      val adyn = quote { query[Person].update(_.name -> "Joe", _.age -> 123) } // Dynamic Insert "assignment form"
      val qdyn = quote { query[Person].update(Person("Joe", 123)) }            // Dynamic Insert entity form

      "dynamic schema makes whole query dynamic - it will plug into runtime queries post-facto" in {
        given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")

        // For static queries `insert` macro is only being evaluated right here so `given sm` will change names
        ctx.run(q).triple mustEqual    ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Dynamic)
        ctx.run(a).triple mustEqual    ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Dynamic)

        // For dynamic queries `insert` macro is already evaluated therefore `given sm` will not change the column names
        ctx.run(qdyn).triple mustEqual ("UPDATE Person SET name = 'Joe', age = 123", List(), Dynamic)
        ctx.run(adyn).triple mustEqual ("UPDATE Person SET name = 'Joe', age = 123", List(), Dynamic)
      }
    }
  }

  // TODO Variation of this with only InsertMeta, and well as both InsertMeta and SchemaMeta (inline, and dynamic)
  "given queries in an outer scope - with the given already there" - {
    given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")
    inline def a = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) } // Insert "assignment form"
    inline def q = quote { query[Person].insert(Person("Joe", 123)) }            // Insert entity form
    val adyn = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) } // Dynamic Insert "assignment form"
    val qdyn = quote { query[Person].insert(Person("Joe", 123)) }            // Dynamic Insert entity form

    // Since schema meta has been plugged in already, all behaviors are the same
    "dynamic schema plugs in and makes all queries dynamic" in {
      ctx.run(q).triple mustEqual    ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Dynamic)
      ctx.run(a).triple mustEqual    ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Dynamic)
      ctx.run(qdyn).triple mustEqual ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Dynamic)
      ctx.run(adyn).triple mustEqual ("INSERT INTO tblPerson (colName,age) VALUES ('Joe', 123)", List(), Dynamic)
    }
  }

  // TODO Variation of this with only InsertMeta, and well as both InsertMeta and SchemaMeta (inline, and dynamic)
  "(update) given queries in an outer scope - with the given already there" - {
    given sm: SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")
    inline def a = quote { query[Person].update(_.name -> "Joe", _.age -> 123) } // Insert "assignment form"
    inline def q = quote { query[Person].update(Person("Joe", 123)) }            // Insert entity form
    val adyn = quote { query[Person].update(_.name -> "Joe", _.age -> 123) } // Dynamic Insert "assignment form"
    val qdyn = quote { query[Person].update(Person("Joe", 123)) }            // Dynamic Insert entity form

    // Since schema meta has been plugged in already, all behaviors are the same
    "dynamic schema plugs in and makes all queries dynamic" in {
      ctx.run(q).triple mustEqual    ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Dynamic)
      ctx.run(a).triple mustEqual    ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Dynamic)
      ctx.run(qdyn).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Dynamic)
      ctx.run(adyn).triple mustEqual ("UPDATE tblPerson SET colName = 'Joe', age = 123", List(), Dynamic)
    }
  }














    // "regular dynamic query shuold work" in {
    //   ctx.run(qdyn).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Dynamic)
    //   ctx.run(adyn).triple mustEqual ("INSERT INTO Person (name,age) VALUES ('Joe', 123)", List(), Dynamic)
    // }

    // TODO add dynamic schema ignore possiblity


  // TODO Need more testing of this for multiple use-cases
//   "Entity with embedding" - {
//     case class Address(street:String, zip:Int) extends Embedded
//     case class Person(name: String, age: Int, address: Address)
//     inline def people = quote { query[Person] }
//     def peopleRuntime = quote { query[Person] }
//   }
}