package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.ReturnAction.ReturnColumns
import io.getquill.context.mirror.Row
import io.getquill.context.sql.testContext
import io.getquill.context.sql.testContext._
import io.getquill._

class InfixSpec extends Spec {
  "infix" - {
    "part of the query - pure" in {
      inline def q = quote {
        qr1.map(t => infix"CONCAT(${t.s}, ${t.s})".pure.as[String])
      }
      testContext.run(q).string mustEqual
        "SELECT CONCAT(t.s, t.s) FROM TestEntity t"
    }
    "part of the query - dynamic" in {
      val q = quote {
        qr1.map(t => infix"CONCAT(${t.s}, ${t.s})".as[String])
      }
      testContext.run(q).string mustEqual
        "SELECT t._1 FROM (SELECT CONCAT(t.s, t.s) AS _1 FROM TestEntity t) AS t"
    }
    "part of the query" in {
      inline def q = quote {
        qr1.map(t => infix"CONCAT(${t.s}, ${t.s})".as[String])
      }
      testContext.run(q).string mustEqual
        "SELECT t._1 FROM (SELECT CONCAT(t.s, t.s) AS _1 FROM TestEntity t) AS t"
    }
    "source query" in {
      case class Entity(i: Int)
      inline def q = quote {
        infix"SELECT 1 i FROM DUAL".as[Query[Entity]].map(a => a.i)
      }
      testContext.run(q).string mustEqual
        "SELECT a.i FROM (SELECT 1 i FROM DUAL) AS a"
    }
    "full infix query" in {
      val result = testContext.run(infix"SELECT * FROM TestEntity".as[Query[TestEntity]])
      result.string mustEqual
        "SELECT x.s, x.i, x.l, x.o, x.b FROM (SELECT * FROM TestEntity) AS x"
    }
    // Not supported yet by regular quill either
    // "full infix action returning" in {
    //   testContext.run(infix"INSERT INTO TestEntity (foo) VALUES (bar) RETURNING baz".as[ActionReturning[String, TestEntity]]).string mustEqual
    //     "INSERT INTO TestEntity (foo) VALUES (bar) RETURNING baz"
    // }
    "full infix action" in {
      testContext.run(infix"DELETE FROM TestEntity".as[Action[TestEntity]]).string mustEqual
        "DELETE FROM TestEntity"
    }
    "full infix action - delete" in {
      testContext.run(infix"DELETE FROM TestEntity".as[Delete[TestEntity]]).string mustEqual
        "DELETE FROM TestEntity"
    }
    "do not nest query if infix starts with input query" in {
      case class Entity(i: Int)
      val forUpdate = quote {
        (q: Query[Entity]) => infix"$q FOR UPDATE".as[Query[Entity]].map(a => a.i)
      }
      testContext.run(forUpdate(query[Entity])).string mustEqual
        "SELECT a.i FROM Entity a FOR UPDATE"
    }

  }
}
