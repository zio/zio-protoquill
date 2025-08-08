package io.getquill.misc

import io.getquill.context.ZioJdbc._
import io.getquill.ZioSpec
import zio.{ Task, ZIO, ZLayer }
import io.getquill._

import javax.sql.DataSource

class ZioJdbcUnderlyingContextSpec extends ZioProxySpec {

  val context = testContext.underlying
  import testContext.underlying._

  case class TestEntity(s: String, i: Int, l: Long, o: Option[Int], b: Boolean)
  inline def qr1 = quote {
    query[TestEntity]
  }

  "provides transaction support" - {
    "success" in {
      (for {
        _ <- testContext.underlying.run(qr1.delete)
        _ <- testContext.underlying.transaction { testContext.underlying.run(qr1.insert(_.i -> 33)) }
        r <- testContext.underlying.run(qr1)
      } yield r).onDataSource.runSyncUnsafe().map(_.i) mustEqual List(33)
    }
    "success - with dependency" in {
      (for {
        _ <- testContext.underlying.run(qr1.delete)
        _ <- testContext.underlying.transaction {
          for {
            env <- ZIO.service[Int]
            qry <- testContext.underlying.run(qr1.insert(_.i -> lift(env)))
          } yield qry
        }
        r <- testContext.underlying.run(qr1)
      } yield r).onSomeDataSource.provideSomeLayer(ZLayer.succeed(33)).runSyncUnsafe().map(_.i) mustEqual List(33)
    }
    "success - stream" in {
      (for {
        _ <- testContext.underlying.run(qr1.delete)
        seq <- testContext.underlying.transaction {
          for {
            _ <- testContext.underlying.run(qr1.insert(_.i -> 33))
            s <- accumulate(testContext.underlying.stream(qr1))
          } yield s
        }
        r <- testContext.underlying.run(qr1)
      } yield (seq.map(_.i), r.map(_.i))).onDataSource.runSyncUnsafe() mustEqual ((List(33), List(33)))
    }
    "failure - nested" in {
      (for {
        _ <- testContext.underlying.run(qr1.delete)
        e <- testContext.underlying.transaction {
          testContext.underlying.run(qr1.insert(_.i -> 36)) *>
            testContext.underlying.transaction {
              ZIO.collectAll(Seq(
                testContext.underlying.run(qr1.insert(_.i -> 18)),
                ZIO.attempt {
                  throw new IllegalStateException
                }
              ))
            }
        }.catchSome {
          case e: Exception => ZIO.attempt(e.getClass.getSimpleName)
        }
        r <- testContext.underlying.run(qr1)
      } yield (e, r.isEmpty)).onDataSource.runSyncUnsafe() mustEqual (("IllegalStateException", true))
    }
    "nested" in {
      (for {
        _ <- testContext.underlying.run(qr1.delete)
        _ <- testContext.underlying.transaction { testContext.underlying.transaction { testContext.underlying.run(qr1.insert(_.i -> 33)) } }
        r <- testContext.underlying.run(qr1)
      } yield r).onDataSource.runSyncUnsafe().map(_.i) mustEqual List(33)
    }
    "keep existing errors" in {
      import java.sql.{Connection, SQLException}

      val zioThatCanFail: ZIO[Any, String, Nothing] = ZIO.fail("Custom Error")
      "val result: ZIO[Connection, String | SQLException, List[TestEntity]] = testContext.underlying.transaction(zioThatCanFail *> testContext.underlying.run(qr1))" must compile
    }
    // "prepare" in {
    //   testContext.underlying.prepareParams(
    //     "select * from Person where name=? and age > ?", (ps, session) => (List("Sarah", 127), ps)
    //   ).onDataSource.runSyncUnsafe() mustEqual List("127", "'Sarah'")
    // }
  }
}
