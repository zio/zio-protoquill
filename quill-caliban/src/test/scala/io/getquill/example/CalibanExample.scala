package io.getquill

import caliban.GraphQL.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.{RootResolver, ZHttpAdapter}
import zhttp.http._
import zhttp.service.Server
import zio.{ExitCode, ZEnv, ZIO}
import io.getquill._
import io.getquill.context.qzio.ImplicitSyntax._
import io.getquill.context.ZioJdbc._
import io.getquill.util.LoadConfig
import zio.Console.printLine
import zio.{ App, ExitCode, URIO, Task }
import java.io.Closeable
import javax.sql.DataSource

import scala.language.postfixOps
import caliban.execution.Field
import caliban.schema.ArgBuilder
import io.getquill.CalibanIntegration._
import io.getquill.util.ContextLogger
import io.getquill
import io.getquill.FlatSchema._


object Dao:
  case class PersonAddressPlanQuery(plan: String, pa: List[PersonAddress])
  private val logger = ContextLogger(classOf[Dao.type])

  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val ds = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource
  given Implicit[DataSource] = Implicit(ds)

  inline def q(inline columns: List[String], inline filters: Map[String, String]) =
    quote {
      query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
        .map((p, a) => PersonAddress(p.id, p.first, p.last, p.age, a.map(_.street)))
        .filterColumns(columns)
        .filterByKeys(filters)
        .take(10)
    }
  inline def plan(inline columns: List[String], inline filters: Map[String, String]) =
    quote { infix"EXPLAIN ${q(columns, filters)}".pure.as[Query[String]] }

  def personAddress(columns: List[String], filters: Map[String, String]) =
    println(s"Getting columns: $columns")
    run(q(columns, filters)).implicitDS.mapError(e => {
      logger.underlying.error("personAddress query failed", e)
      e
    })

  def personAddressPlan(columns: List[String], filters: Map[String, String]) =
    run(plan(columns, filters), OuterSelectWrap.Never).map(_.mkString("\n")).implicitDS.mapError(e => {
      logger.underlying.error("personAddressPlan query failed", e)
      e
    })

  def resetDatabase() =
    (for {
      _ <- run(infix"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
      _ <- run(liftQuery(ExampleData.people).foreach(row => query[PersonT].insertValue(row)))
      _ <- run(liftQuery(ExampleData.addresses).foreach(row => query[AddressT].insertValue(row)))
    } yield ()).implicitDS
end Dao

object CalibanExample extends zio.App:

  case class Queries(
      personAddress: Field => (ProductArgs[PersonAddress] => Task[List[PersonAddress]]),
      personAddressPlan: Field => (ProductArgs[PersonAddress] => Task[Dao.PersonAddressPlanQuery])
  )

  val endpoints =
     graphQL(
      RootResolver(
        Queries(
          personAddress =>
            (productArgs =>
              Dao.personAddress(quillColumns(personAddress), productArgs.keyValues)
            ),
          personAddressPlan =>
            (productArgs => {
              val cols = quillColumns(personAddressPlan)
              (Dao.personAddressPlan(cols, productArgs.keyValues) zip Dao.personAddress(cols, productArgs.keyValues)).map(
                (pa, plan) => Dao.PersonAddressPlanQuery(pa, plan)
              )
            })
        )
      )
    ).interpreter

  val myApp = for {
    _ <- Dao.resetDatabase()
    interpreter <- endpoints
    _ <- Server.start(
        port = 8088,
        http = Http.route[Request] { case _ -> Root / "api" / "graphql" =>
          ZHttpAdapter.makeHttpService(interpreter)
        }
      )
      .forever
  } yield ()

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    myApp.exitCode

end CalibanExample