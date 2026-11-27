package zio.quill.context.cassandra

import zio.quill._
import zio.quill.context.ExecutionInfo
import zio.quill.ReturnAction.ReturnNothing

import scala.concurrent.ExecutionContext.Implicits.{ global => ec }
import scala.util.{ Success, Try }

class CassandraContextSpec extends Spec {
  // val et = zio.quill.context.ExecutionType
  // val vvv = et.lifts
  val unknown = ExecutionInfo(zio.quill.context.ExecutionType.Static, zio.quill.ast.NullValue, zio.quill.quat.Quat.Value)

  "run non-batched action" - {

    "async" in {
      import testAsyncDB._
      case class TestEntity(id: Int, s: String, i: Int, l: Long, o: Int)
      inline def update = quote {
        query[TestEntity].filter(_.id == lift(1)).update(_.i -> lift(1))
      }
      await(testAsyncDB.run(update)) mustEqual (())
    }
    "sync" in {
      import testSyncDB._
      case class TestEntity(id: Int, s: String, i: Int, l: Long, o: Int)
      inline def update = quote {
        query[TestEntity].filter(_.id == lift(1)).update(_.i -> lift(1))
      }
      testSyncDB.run(update) mustEqual (())
    }
  }

  "fail on returning" in {
    import testSyncDB._
    val p: Prepare = (x, session) => (Nil, x)
    val e: Extractor[Int] = (_, _) => 1

      intercept[IllegalStateException](executeBatchActionReturning(Nil, e)(unknown, ec)).getMessage
  }

  "probe" in {
    testSyncDB.probe("SELECT * FROM TestEntity") mustBe Success(())
  }

  "return failed future on `prepare` error in async context" - {
    "query" - {
      val f = testAsyncDB.executeQuery("bad cql", (p, s) => (List(), p), (r, s) => ())(unknown, ec)
      Try(await(f)).isFailure mustEqual true
      ()
    }
    "action" - {
      val f = testAsyncDB.executeAction("bad cql", (p, s) => (List(), p))(unknown, ec)
      Try(await(f)).isFailure mustEqual true
      ()
    }
  }
}
