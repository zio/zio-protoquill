package io.getquill.ported.sqlidiomspec

import scala.language.implicitConversions
import io.getquill.context.sql.testContext
import io.getquill.context.sql.testContext._
import io.getquill._

/**
 * Reproduction tests for critical correctness issues.
 * Each test documents the issue number, the expected behavior, and the actual (buggy) behavior.
 */
class CriticalIssuesSpec extends Spec {

  // ==================== #396 ====================
  // Flat join in for-comprehension with outer .filter() generates wrong column reference in WHERE clause.
  // WHERE fc."_1" = 1 instead of WHERE f."file_key" = 1 (or equivalent)
  "#396 - flat join with outer filter should reference correct columns" in {
    case class File(fileKey: Long, fileCategoryKey: Int)
    case class FileCategory(fileCategoryKey: Int)

    inline def q = quote {
      (for {
        f <- query[File]
        fc <- query[FileCategory].join(fc => fc.fileCategoryKey == f.fileCategoryKey)
      } yield (f.fileKey, fc.fileCategoryKey))
        .filter(_._1 == 1L)
    }

    val sql = testContext.run(q).string
    // The WHERE clause should reference the file table's fileKey, not a synthetic _1 on the wrong table
    sql must not include ("._1")
    // Sanity: should still be a join query
    sql must include ("INNER JOIN")
  }

  // ==================== #239 ====================
  // Flat join in for-comprehension with filter on tuple references the wrong table alias.
  // WHERE bar.someField = 'baz' instead of WHERE foo.someField = 'baz'
  "#239 - flat join filter should reference correct table in equality" in {
    case class Foo(id: Int, someField: String)
    case class Bar(fooId: Int, otherField: String)

    inline def q = quote {
      (for {
        foo <- query[Foo]
        bar <- query[Bar].join(bar => bar.fooId == foo.id)
      } yield (foo, bar))
        .filter { case (foo, bar) =>
          foo.someField == "baz" && bar.otherField == "boo"
        }
    }

    val sql = testContext.run(q).string
    // foo.someField should be on the foo table, not the bar table
    sql must not include ("bar.someField = 'baz'")
    sql must not include ("bar.some_field = 'baz'")
  }

  // ==================== #335 ====================
  // Recursive inline query reuse generates wrong column references in subqueries.
  // When a query maps entities into (scalar, CaseClass) tuples via sql"..." (which
  // prevents optimizer flattening), columns get aliased as _1, _2field1, _2field2 etc.
  // In a self-referencing subquery, accessing .field1 generates bare i1.field1
  // instead of i1._2field1, which doesn't exist in the subquery scope.
  "#335 - recursive inline query reuse should generate correct column references" in {
    case class Item(id: Int, score: Int, category: Int)

    // Single quote block to avoid cross-quote reference issues.
    // sql"..." prevents the optimizer from flattening the tuple mapping.
    inline def q = quote {
      query[Item]
        .map(i => (sql"${i.score} + 1".as[Int], i))
        .filter(_._1 > 0)
        .map(_._2)
        .filter(x =>
          query[Item]
            .map(i => (sql"${i.score} + 1".as[Int], i))
            .filter(_._1 > 0)
            .map(_._2)
            .filter(y => y.category == x.category && y.id != x.id)
            .map(_.score)
            .max
            .exists(_ < x.score)
        )
    }

    val sql = testContext.run(q).string
    // BUG: generates MAX(i1.score) but in the subquery scope the column
    // is aliased as _2score, so it should be MAX(i1._2score)
    sql must not include ("MAX(i1.score)")
  }

  // ==================== #431 ====================
  // Batch insert with an empty collection generates invalid SQL: INSERT INTO ... VALUES (with no values)
  "#431 - batch insert with empty collection should not generate invalid SQL" in {
    inline def q = quote {
      liftQuery(List.empty[TestEntity]).foreach(e => query[TestEntity].insertValue(e))
    }

    // Should either produce no SQL or handle gracefully, not "INSERT INTO ... VALUES "
    val result = testContext.run(q)
    val groups = result.groups
    // With an empty list, there should be no groups to execute
    groups mustBe empty
  }

  // ==================== #412 ====================
  // Double lift: lift(lift(x)) previously compiled but failed at runtime.
  // Now caught at compile time with a clear error message.
  "#412 - double lift should be rejected at compile time" in {
    """
      import io.getquill.context.sql.testContext
      import io.getquill.context.sql.testContext._
      import io.getquill._
      val x = 1
      inline def q = quote {
        query[TestEntity].filter(t => t.i == lift(lift(x)))
      }
      testContext.run(q)
    """ mustNot compile
  }
}
