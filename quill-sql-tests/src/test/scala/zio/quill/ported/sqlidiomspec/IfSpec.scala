package zio.quill.ported.sqlidiomspec

import scala.language.implicitConversions
import zio.quill.ReturnAction.ReturnColumns
//import zio.quill.MirrorSqlDialectWithReturnMulti
import zio.quill.context.mirror.Row
import zio.quill.context.sql.testContext
import zio.quill.context.sql.testContext._
import zio.quill._

class IfSpec extends Spec {
  "if" - {
    "simple" in {
      inline def q = quote {
        qr1.map(t => if (t.i > 0) "a" else "b")
      }
      testContext.run(q).string mustEqual
        "SELECT CASE WHEN t.i > 0 THEN 'a' ELSE 'b' END FROM TestEntity t"
    }
    "simple booleans" in {
      inline def q = quote {
        qr1.map(t => if (true) true else false)
      }
      testContext.run(q).string mustEqual
        // Originally was this:
        //"SELECT CASE WHEN true THEN true ELSE false END FROM TestEntity t"
        // But scala itself beta reduces the booleans
        "SELECT true FROM TestEntity t"
    }
    "nested" in {
      inline def q = quote {
        qr1.map(t => if (t.i > 0) "a" else if (t.i > 10) "b" else "c")
      }
      testContext.run(q).string mustEqual
        "SELECT CASE WHEN t.i > 0 THEN 'a' WHEN t.i > 10 THEN 'b' ELSE 'c' END FROM TestEntity t"
    }
    "nested booleans" in {
      inline def q = quote {
        qr1.map(t => if (true) true else if (true) true else false)
      }
      testContext.run(q).string mustEqual (
        // Originally was this
        //"SELECT CASE WHEN true THEN true WHEN true THEN true ELSE false END FROM TestEntity t"
        // but scala beta reduces to this:
        "SELECT true FROM TestEntity t"
      )
    }
    "nested booleans inline" in {
      inline def q = quote {
        qr1.map(t => if (lazyLift(true)) true else if (lift(true)) true else false)
      }
      testContext.run(q).string mustEqual (
        "SELECT CASE WHEN ? THEN true WHEN ? THEN true ELSE false END FROM TestEntity t"
      )
    }
  }
}