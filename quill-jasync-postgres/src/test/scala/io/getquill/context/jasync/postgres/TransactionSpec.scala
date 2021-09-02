package io.getquill.context.jasync.postgres

import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import io.getquill.context.sql.PeopleSpec
import io.getquill._
import scala.util.Try
import scala.util.Success

class TransactionSpec extends PeopleSpec {

  val context = testContext
  import testContext._

  override def beforeAll() =
    await {
      testContext.transaction { implicit ec =>
        for {
          _ <- testContext.run(query[Couple].delete)
          _ <- testContext.run(query[Person].delete)
        } yield {}
      }
    }

  "transaction failure" in {
    await {
      testContext.transaction { implicit ec =>
        for {
          _ <- testContext.run(query[Couple].delete)
          _ <- testContext.run(query[Person].delete)
        } yield {}
      }
    }
    Try {
      await {
        testContext.transaction { implicit ec =>
          for {
            _ <- testContext.run(query[Couple].insert(lift(Couple("Alex", "Bert"))))
            _ <- scala.concurrent.Future { throw new RuntimeException("Blahblahblah") }
            _ <- testContext.run(query[Person].insert(lift(Person("Alex", 60))))
          } yield {}
        }
      }
    } match {
      case Success(value) => fail("Query with failure should not succeed")
      case _              => println("Expected failure reached")
    }
    await(testContext.run(query[Couple])) mustEqual List()
    await(testContext.run(query[Person])) mustEqual List()
  }

  "transaction success" in {
    await {
      testContext.transaction { implicit ec =>
        for {
          _ <- testContext.run(query[Couple].delete)
          _ <- testContext.run(query[Person].delete)
        } yield {}
      }
    }
    await {
      testContext.transaction { implicit ec =>
        for {
          _ <- testContext.run(query[Couple].insert(lift(Couple("Alex", "Bert"))))
          _ <- testContext.run(query[Person].insert(lift(Person("Alex", 60))))
        } yield {}
      }
    }

    await(testContext.run(query[Couple])) mustEqual List(Couple("Alex", "Bert"))
    await(testContext.run(query[Person])) mustEqual List(Person("Alex", 60))
  }
}
