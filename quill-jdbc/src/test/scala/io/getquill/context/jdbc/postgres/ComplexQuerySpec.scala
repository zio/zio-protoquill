package io.getquill.context.jdbc.postgres

import io.getquill.Spec
import org.scalatest.BeforeAndAfter
import io.getquill.Ord
import io.getquill._
import org.scalatest._

class ComplexQuerySpec extends Spec with TestEntities with BeforeAndAfter with PostgresJdbcContext.Codec {

  val context = testContext
  import testContext._

  before {
    testContext.run(query[TestEntity].delete)
    testContext.run(query[TestEntity2].delete)
  }

  "complex queries" - {

    // if you make this not inline you get the following error:
    // The InsertMeta form is invalid. It is Pointable: testEntity2InsertMeta. It must be either Uprootable or Pluckable i.e. it has at least a UID that can be identified.
    // need to improve the error
    implicit inline def testEntity2InsertMeta: InsertMeta[TestEntity2] = insertMeta[TestEntity2](_.o)

    inline def testEntityInsert =
      quote((p: TestEntity) => query[TestEntity].insertValue(p))

    inline def testEntity2Insert =
      quote((p: TestEntity2) => query[TestEntity2].insertValue(p))

    "join + nesting + infixes" in {

      val testEntities = List(
        TestEntity("aaa", 11, 1L, None, true),
        TestEntity("aaa", 12, 1L, None, true),
        TestEntity("aaa", 13, 3L, None, true),
        TestEntity("aaa", 14, 3L, None, true),
        TestEntity("bbb", 15, 4L, None, true),
        TestEntity("bbb", 16, 4L, None, true)
      )

      val testEntities2 = List(
        TestEntity2("aaf", 1, 1L, None),
        TestEntity2("aaf", 2, 1L, None),
        TestEntity2("aaf", 3, 3L, None),
        TestEntity2("aaf", 4, 3L, None),
        TestEntity2("baf", 5, 3L, None),
        TestEntity2("baf", 6, 2L, None)
      )

      testContext.run(liftQuery(testEntities).foreach(p => testEntityInsert(p)))
      testContext.run(liftQuery(testEntities2).foreach(p => testEntity2Insert(p)))

      inline def q = quote {
        query[TestEntity]
          .join(query[TestEntity2])
          .on { case (one, two) => one.l == two.l }
          .filter(_._1.s == "aaa")
          .map(_._2)
          .sortBy(t => (t.s, t.i))(Ord.desc)
          .map(e => (sql"DISTINCT ON (${e.s}) ${e.s}".as[String], e.i))
          .filter(r => r._2 == 3 || r._2 == 4 || r._2 == 5)
      }

      testContext.run(q) mustEqual (List(("baf", 5), ("aaf", 4)))
    }
  }

}
