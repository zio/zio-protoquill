package io.getquill.context.cassandra.zio.examples

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
import zio.console.putStrLn
import zio.{ App, ExitCode, Has, URIO, Task }
import java.io.Closeable
import javax.sql.DataSource

import scala.language.postfixOps
import caliban.execution.Field
import caliban.schema.ArgBuilder
import io.getquill.CalibanIntegration._

case class PersonT(id: Int, name: String, age: Int)
case class AddressT(ownerId: Int, street: String)
case class PersonAddress(id: Int, name: String, age: Int, street: Option[String])

case class PersonAddressPlanQuery(plan: String, pa: List[PersonAddress])

object Dao:

  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val ds = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource
  given Implicit[Has[DataSource with Closeable]] = Implicit(Has(ds))

  inline def q(inline columns: List[String], inline filters: Map[String, String]) =
    quote {
      query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
        .map((p, a) => PersonAddress(p.id, p.name, p.age, a.map(_.street)))
        .filterColumns(columns)
        .filterByKeys(filters)
        .take(10)
    }
  inline def plan(inline columns: List[String], inline filters: Map[String, String]) =
    quote { infix"EXPLAIN ${q(columns, filters)}".pure.as[Query[String]] }

  def personAddress(columns: List[String], filters: Map[String, String]) =
    run(q(columns, filters)).implicitDS.mapError(e => {
      println("===========ERROR===========" + e.getMessage) //
      e
    })

  def personAddressPlan(columns: List[String], filters: Map[String, String]) =
    run(plan(columns, filters), OuterSelectWrap.Never).map(_.mkString("\n")).implicitDS.mapError(e => {
      println("===========ERROR===========" + e.getMessage) //helloooo
      e
    })
end Dao

case class Queries(
    personAddress: Field => (ProductArgs[PersonAddress] => Task[List[PersonAddress]]),
    personAddressPlan: Field => (ProductArgs[PersonAddress] => Task[PersonAddressPlanQuery])
)
object CalibanExample extends zio.App {

  val myApp = for {
    interpreter <- graphQL(
      RootResolver(
        Queries(
          personAddress =>
            (productArgs =>
              Dao.personAddress(personAddress.fields.map(_.name), productArgs.keyValues)
            ),
          personAddressPlan =>
            (productArgs => {
              val cols = personAddressPlan.fields.flatMap(_.fields.map(_.name))
              println(s"==== Selected Columns: ${cols}")
              //println(s"==== Nested Fields: ${personAddressPlan.fields.map(_.fields.map(_.name))}")
              (Dao.personAddressPlan(cols, productArgs.keyValues) zip Dao.personAddress(cols, productArgs.keyValues)).map(
                (pa, plan) => PersonAddressPlanQuery(pa, plan)
              )
            })
        )
      )
    ).interpreter
    _ <- Server
      .start(
        port = 8088,
        http = Http.route { case _ -> Root / "api" / "graphql" =>
          ZHttpAdapter.makeHttpService(interpreter)
        }
      )
      .forever
  } yield ()

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    myApp.exitCode

}
