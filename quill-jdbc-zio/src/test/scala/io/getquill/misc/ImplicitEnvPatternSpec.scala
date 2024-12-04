package io.getquill.misc

import io.getquill.{ JdbcContextConfig, PeopleZioSpec }

import java.io.Closeable
import javax.sql.DataSource
import io.getquill.context.qzio.ImplicitSyntax._
import io.getquill.util.LoadConfig
import zio.ZIO
import io.getquill._

class ImplicitEnvPatternSpec extends PeopleZioProxySpec with PostgresJdbcContext.Codec {

  // Need to specify prefix to use for the setup

  val context = testContext
  import testContext._

  override def beforeAll() = {
    super.beforeAll()
    testContext.transaction {
      for {
        _ <- testContext.run(query[Couple].delete)
        _ <- testContext.run(query[Person].filter(_.age > 0).delete)
        _ <- testContext.run(liftQuery(peopleEntries).foreach(p => peopleInsert(p)))
        _ <- testContext.run(liftQuery(couplesEntries).foreach(p => couplesInsert(p)))
      } yield ()
    }.runSyncUnsafe()
  }

  case class MyService(ds: DataSource) {
    implicit val env: Implicit[DataSource] = Implicit(ds)

    def alexes = testContext.run(query[Person].filter(p => p.name == "Alex"))
    def berts = testContext.run(query[Person].filter(p => p.name == "Bert"))
    def coras = testContext.run(query[Person].filter(p => p.name == "Cora"))
  }

  def makeDataSource() = io.getquill.postgres.pool

  "dataSource based context should fetch results" in {
    val (alexes, berts, coras) =
      ZIO.scoped {
        ZIO.attempt(makeDataSource()).flatMap { ds =>
          for {
            svc <- ZIO.attempt(MyService(ds))
            alexes <- svc.alexes
            berts <- svc.berts
            coras <- svc.coras
          } yield (alexes, berts, coras)
        }
      }.runSyncUnsafe()

    alexes must contain theSameElementsAs (peopleEntries.filter(_.name == "Alex"))
    berts must contain theSameElementsAs (peopleEntries.filter(_.name == "Bert"))
    coras must contain theSameElementsAs (peopleEntries.filter(_.name == "Cora"))
  }

}
