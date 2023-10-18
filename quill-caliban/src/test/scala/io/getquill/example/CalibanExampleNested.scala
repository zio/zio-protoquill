package io.getquill

import caliban.GraphQL.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.{RootResolver, ZHttpAdapter}
import zhttp.http._
import zhttp.service.Server
import zio.{ExitCode, ZIO}
import io.getquill._
import io.getquill.context.qzio.ImplicitSyntax._
import io.getquill.context.ZioJdbc._
import io.getquill.util.LoadConfig
import zio.Console.printLine
import zio.{ZIOApp, ExitCode, URIO, Task}
import java.io.Closeable
import javax.sql.DataSource

import scala.language.postfixOps
import caliban.execution.Field
import caliban.schema.ArgBuilder
import io.getquill.CalibanIntegration._
import io.getquill.util.ContextLogger
import io.getquill.NestedSchema._

object DaoNested {
  case class PersonAddressPlanQuery(plan: String, pa: List[PersonAddressNested])
  private val logger = ContextLogger(classOf[DaoNested.type])

  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val ds                = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource
  given Implicit[DataSource] = Implicit(ds)

  inline def q(inline columns: List[String], inline filters: Map[String, String]) =
    quote {
      query[PersonT]
        .leftJoin(query[AddressT])
        .on((p, a) => p.id == a.ownerId)
        .map((p, a) => PersonAddressNested(p.id, p.name, p.age, a.map(_.street)))
        .filterColumns(columns)
        .filterByKeys(filters)
        .take(10)
    }
  inline def plan(inline columns: List[String], inline filters: Map[String, String]) =
    quote(sql"EXPLAIN ${q(columns, filters)}".pure.as[Query[String]])

  def personAddress(columns: List[String], filters: Map[String, String]) = {
    println(s"Getting columns: $columns")
    run(q(columns, filters)).implicitDS.mapError { e =>
      logger.underlying.error("personAddress query failed", e)
      e
    }
  }

  def personAddressPlan(columns: List[String], filters: Map[String, String]) =
    run(plan(columns, filters), OuterSelectWrap.Never).map(_.mkString("\n")).implicitDS.mapError { e =>
      logger.underlying.error("personAddressPlan query failed", e)
      e
    }

  def resetDatabase() =
    (for {
      _ <- run(sql"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
      _ <- run(liftQuery(ExampleData.people).foreach(row => query[PersonT].insertValue(row)))
      _ <- run(liftQuery(ExampleData.addresses).foreach(row => query[AddressT].insertValue(row)))
    } yield ()).implicitDS
} // end DaoNested

object CalibanExampleNested extends zio.ZIOAppDefault {
  private val logger = ContextLogger(classOf[CalibanExampleNested.type])

  case class Queries(
    personAddress: Field => (ProductArgs[PersonAddressNested] => Task[List[PersonAddressNested]]),
    personAddressPlan: Field => (ProductArgs[PersonAddressNested] => Task[DaoNested.PersonAddressPlanQuery])
  )

  val endpoints =
    graphQL(
      RootResolver(
        Queries(
          personAddress => (productArgs => DaoNested.personAddress(quillColumns(personAddress), productArgs.keyValues)),
          personAddressPlan =>
            (productArgs => {
              val cols = quillColumns(personAddressPlan)
              logger.underlying.info(s"Selected Columns: ${cols}")
              (DaoNested.personAddressPlan(cols, productArgs.keyValues) zip DaoNested.personAddress(
                cols,
                productArgs.keyValues
              )).map((pa, plan) => DaoNested.PersonAddressPlanQuery(pa, plan))
            })
        )
      )
    ).interpreter

  val myApp = for {
    _           <- DaoNested.resetDatabase()
    interpreter <- endpoints
    _ <- Server
           .start(
             port = 8088,
             http = Http.collectHttp[Request] { case _ -> !! / "api" / "graphql" =>
               ZHttpAdapter.makeHttpService(interpreter)
             }
           )
           .forever
  } yield ()

  override def run =
    myApp.exitCode

} // end CalibanExampleNested
