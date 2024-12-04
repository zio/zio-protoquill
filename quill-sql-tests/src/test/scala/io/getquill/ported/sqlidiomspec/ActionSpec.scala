package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.ReturnAction.ReturnColumns
//import io.getquill.MirrorSqlDialectWithReturnMulti
import io.getquill.context.mirror.Row
import io.getquill.context.sql.testContext
import io.getquill.context.sql.testContext._
import io.getquill._
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.context.ExecutionType

class ActionSpec extends MirrorSpec {
  "action" - {
    "insert" - {
      "not affected by variable name" - {
        "simple - dynamic" in {
          val q = quote { (v: TestEntity) =>
            query[TestEntity].insertValue(v)
          }
          val v = TestEntity("s", 1, 2L, Some(1), true)
          testContext.run(q(lift(v))).string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)"
        }
        "simple" in {
          inline def q = quote { (v: TestEntity) =>
            query[TestEntity].insertValue(v)
          }
          val v = TestEntity("s", 1, 2L, Some(1), true)
          testContext.run(q(lift(v))).string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)"
        }
        "returning" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
          import ctx._
          val q = quote { (v: TestEntity) =>
            query[TestEntity].insertValue(v)
          }
          val v = TestEntity("s", 1, 2L, Some(1), true)
          ctx.run(q(lift(v)).returning(v => v.i)).string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)"
        }
        "returning generated" in {
          val q = quote { (v: TestEntity) =>
            query[TestEntity].insertValue(v)
          }
          val v = TestEntity("s", 1, 2L, Some(1), true)
          testContext.run(q(lift(v)).returningGenerated(v => v.i)).string mustEqual "INSERT INTO TestEntity (s,l,o,b) VALUES (?, ?, ?, ?)"
        }
        "foreach" in {
          val v = TestEntity("s", 1, 2L, Some(1), true)
          val groups =
            testContext.run(
              liftQuery(List(v)).foreach(v => query[TestEntity].insertValue(v))
            ).groups mustEqual
              List((
                "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)",
                List(Row("_1" -> "s", "_2" -> 1, "_3" -> 2L, "_4" -> Some("_1" -> 1), "_5" -> true))
              )
            )
        }
        "foreach returning" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
          import ctx._
          val v = TestEntity("s", 1, 2L, Some(1), true)
          val result = ctx.run(liftQuery(List(v)).foreach(v => query[TestEntity].insertValue(v).returning(v => v.i)))
          result.groups(0)._1 mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)"

          // mustEqual
          //   List(("INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)",
          //     ReturnColumns(List("i")),
          //     List(Row(v.productIterator.toList: _*))
          //   ))
        }
        "foreach returning generated" in {
          val v = TestEntity("s", 1, 2L, Some(1), true)
          val result = testContext.run(
            liftQuery(List(v)).foreach(v => query[TestEntity].insertValue(v).returningGenerated(v => v.i))
          )
          result.groups(0)._1 mustEqual "INSERT INTO TestEntity (s,l,o,b) VALUES (?, ?, ?, ?)"

          // mustEqual
          //   List(("INSERT INTO TestEntity (s,l,o,b) VALUES (?, ?, ?, ?)",
          //     ReturnColumns(List("i")),
          //     List(Row(v.productIterator.toList.filter(m => !m.isInstanceOf[Int]): _*))
          //   ))
        }
      }
      "simple" in { //helloooo
        val q = quote {
          qr1.insert(_.i -> 1, _.s -> "s")
        }
        testContext.run(q).string mustEqual
          "INSERT INTO TestEntity (i,s) VALUES (1, 's')"
      }
      "using nested select" in {
        val q = quote {
          qr1.insert(_.l -> qr2.map(t => t.i).size, _.s -> "s")
        }
        testContext.run(q).string mustEqual
          "INSERT INTO TestEntity (l,s) VALUES ((SELECT COUNT(t.i) FROM TestEntity2 t), 's')"
      }
      "returning" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          query[TestEntity].insertValue(lift(TestEntity("s", 1, 2L, Some(1), true))).returning(_.l)
        }
        val run = ctx.run(q).string mustEqual
          "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)"
      }
      "returning generated" in {
        val q = quote {
          query[TestEntity].insertValue(lift(TestEntity("s", 1, 2L, Some(1), true))).returningGenerated(_.l)
        }
        val run = testContext.run(q).string mustEqual
          "INSERT INTO TestEntity (s,i,o,b) VALUES (?, ?, ?, ?)"
      }
      "returning with single column table" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr4.insertValue(lift(TestEntity4(0))).returning(_.i)
        }
        ctx.run(q).string mustEqual
          "INSERT INTO TestEntity4 (i) VALUES (?)"
      }
      "returning generated with single column table" in {
        val q = quote {
          qr4.insertValue(lift(TestEntity4(0))).returningGenerated(_.i)
        }
        testContext.run(q).string mustEqual
          "INSERT INTO TestEntity4 DEFAULT VALUES"
      }
    }
    "inline updateValue " - {
      "entity" in {
        inline def q = quote {
          qr1.filter(t => t.s == "s").updateValue(TestEntity("s", 1, 2L, Some(1), true))
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity AS t SET s = 's', i = 1, l = 2, o = 1, b = true WHERE t.s = 's'"
      }
      "entity with filter" in {
        inline def q = quote {
          qr1.filter(t => t.s == "s").updateValue(TestEntity("s", 1, 2L, Some(1), true))
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity AS t SET s = 's', i = 1, l = 2, o = 1, b = true WHERE t.s = 's'"
      }
      "entity with filter and lift" in {
        inline def q = quote {
          qr1.filter(t => t.s == lift("s")).updateValue(TestEntity("s", 1, 2L, Some(1), true))
        }
        testContext.run(q).triple mustEqual
          ("UPDATE TestEntity AS t SET s = 's', i = 1, l = 2, o = 1, b = true WHERE t.s = ?", List("s"), Static)
      }
    }
    "updateValue" - {
      val v = TestEntity("s", 1, 2L, Some(1), true)
      "with filter" in {
        inline def q = quote {
          qr1.filter(t => t.s == "s").updateValue(lift(v))
        }
        val result = testContext.run(q)
        result.triple mustEqual
          ("UPDATE TestEntity AS t SET s = ?, i = ?, l = ?, o = ?, b = ? WHERE t.s = 's'", List("s", 1, 2L, Some(1), true), Static)
      }
      "with filter and lift" in {
        inline def q = quote {
          qr1.filter(t => t.s == lift("s")).updateValue(lift(v))
        }
        val result = testContext.run(q)
        result.triple mustEqual
          ("UPDATE TestEntity AS t SET s = ?, i = ?, l = ?, o = ?, b = ? WHERE t.s = ?", List("s", 1, 2L, Some(1), true, "s"), Static)
      }
      "quoted with filter and lift" in {
        inline def orig = quote {
          qr1.filter(t => t.s == lift("s"))
        }
        inline def q = quote {
          orig.updateValue(lift(v))
        }
        val result = testContext.run(q)
        result.triple mustEqual
          ("UPDATE TestEntity AS t SET s = ?, i = ?, l = ?, o = ?, b = ? WHERE t.s = ?", List("s", 1, 2L, Some(1), true, "s"), Static)
      }
      "quoted dynamic with filter and lift" in {
        val orig = quote {
          qr1.filter(t => t.s == lift("s"))
        }
        inline def q = quote {
          orig.updateValue(lift(v))
        }
        val result = testContext.run(q)
        result.triple mustEqual
          ("UPDATE TestEntity AS t SET s = ?, i = ?, l = ?, o = ?, b = ? WHERE t.s = ?", List("s", 1, 2L, Some(1), true, "s"), Dynamic)
      }
      "fully dynamic with filter and lift" in {
        val orig = quote {
          qr1.filter(t => t.s == lift("s"))
        }
        val q = quote {
          orig.updateValue(lift(v))
        }
        testContext.run(q).triple mustEqual
          ("UPDATE TestEntity AS t SET s = ?, i = ?, l = ?, o = ?, b = ? WHERE t.s = ?", List("s", 1, 2L, Some(1), true, "s"), Dynamic)
      }
    }
    "update" - {
      "with filter - null" in {
        inline def q = quote {
          qr1.filter(t => t.s == null).update(_.s -> "s")
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity AS t SET s = 's' WHERE t.s IS NULL"
      }
      "with filter" in {
        inline def q = quote {
          qr1.filter(t => t.s == "s").update(_.s -> "s")
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity AS t SET s = 's' WHERE t.s = 's'"
      }
      "with filter and lift" in {
        inline def q = quote {
          qr1.filter(t => t.s == lift("s")).update(_.s -> "s")
        }
        val result = testContext.run(q)
        result.triple mustEqual
          ("UPDATE TestEntity AS t SET s = 's' WHERE t.s = ?", List("s"), Static)
      }
      "without filter" in {
        val q = quote {
          qr1.update(_.s -> "s")
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity SET s = 's'"
      }
      "using a table column" in {
        val q = quote {
          qr1.update(t => t.i -> (t.i + 1))
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity SET i = (i + 1)"
      }
      "using nested select" in {
        val q = quote {
          qr1.update(_.l -> qr2.map(t => t.i).size)
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity SET l = (SELECT COUNT(t.i) FROM TestEntity2 t)"
      }
    }
    "delete" - {
      "with filter" in {
        val q = quote {
          qr1.filter(t => t.s == null).delete
        }
        testContext.run(q).string mustEqual
          "DELETE FROM TestEntity AS t WHERE t.s IS NULL"
      }
      "without filter" in {
        val q = quote {
          qr1.delete
        }
        testContext.run(q).string mustEqual
          "DELETE FROM TestEntity"
      }
    }
  }

  "no lift in values-clause" in {
    case class Person(name: String, age: Int)
    val list = List("U%", "I%")
    inline def q = quote {
      liftQuery(list).foreach { pat =>
        query[Person].filter(_.name like pat).update(_.name -> "foo")
      }
    }
    testContext.run(q).triple mustEqual (
      "UPDATE Person AS x14 SET name = 'foo' WHERE x14.name like ?",
      List(List("U%"), List("I%")),
      ExecutionType.Static
    )
  }

  "deep nested case class" in {
    case class Name(first: String, last: String)
    case class Contact(name: Option[Name], age: Int)
    val contactsExpected = List(Contact(Some(Name("Joe", "Bloggs")), 123), Contact(None, 456))
    inline def q = quote {
      query[Contact]
        .filter(p => p.name.map(_.first) == Option("foo"))
        .update(_.name.map(_.last) -> Option("bar"))
    }
    testContext.run(q).string mustEqual
      "UPDATE Contact AS p SET last = 'bar' WHERE p.first = 'foo'"
  }

  "deep nested case class - renamed fields" in {
    case class Name(first: String, last: String)
    case class ContactTable(name: Option[Name], age: Int)
    val contactsExpected = List(ContactTable(Some(Name("Joe", "Bloggs")), 123), ContactTable(None, 456))
    inline def contacts = quote {
      querySchema[ContactTable]("Contact", _.name.map(_.first) -> "firstName", _.name.map(_.last) -> "lastName")
    }
    inline def q = quote {
      contacts
        .filter(p => p.name.map(_.first) == Option("foo"))
        .update(_.name.map(_.last) -> Option("bar"))
    }
    testContext.run(q).string mustEqual
      "UPDATE Contact AS p SET lastName = 'bar' WHERE p.firstName = 'foo'"
  }
}