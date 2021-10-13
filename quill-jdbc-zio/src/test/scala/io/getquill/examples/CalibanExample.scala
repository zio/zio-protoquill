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

sealed trait Role

object Role {
  case object SoftwareDeveloper       extends Role
  case object SiteReliabilityEngineer extends Role
  case object DevOps                  extends Role
}

case class Employee(
    name: String,
    role: Role
)

case class EmployeesArgs(role: Role)
case class EmployeeArgs(name: String)

case class PersonT(id: Int, name: String, age: Int)
case class AddressT(ownerId: Int, street: String)
case class PersonAddress(id: Int, name: String, age: Int, street: Option[String])

case class PersonAddressPlanQuery(plan: String, pa: List[PersonAddress])

object Dao:
  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val ds = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource
  given Implicit[Has[DataSource with Closeable]] = Implicit(Has(ds))

  inline def q(inline columns: List[String]) =
    quote {
      query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
        .map((p, a) => PersonAddress(p.id, p.name, p.age, a.map(_.street)))
        .filterColumns(columns)
        .take(10)
    }
  inline def plan(inline columns: List[String]) =
    quote { infix"EXPLAIN ${q(columns)}".pure.as[Query[String]] }

  def personAddress(columns: List[String]) =
    run(q(columns)).implicitDS

  def personAddressPlan(columns: List[String]) =
    run(plan(columns), OuterSelectWrap.Never).map(_.mkString("\n")).implicitDS
end Dao

case class Queries(
    @GQLDescription("Return all employees with specific role")
    employees: EmployeesArgs => List[Employee],
    @GQLDescription("Find an employee by its name")
    employee: EmployeeArgs => Option[Employee],

    personAddress: Field => Task[List[PersonAddress]],
    personAddressPlan: Field => Task[PersonAddressPlanQuery]
)
object CalibanExample extends zio.App {

  val employees = List(
    Employee("Alex", Role.DevOps),
    Employee("Maria", Role.SoftwareDeveloper),
    Employee("James", Role.SiteReliabilityEngineer),
    Employee("Peter", Role.SoftwareDeveloper),
    Employee("Julia", Role.SiteReliabilityEngineer),
    Employee("Roberta", Role.DevOps)
  )

  val myApp = for {
    interpreter <- graphQL(
      RootResolver(
        Queries(
          args => employees.filter(e => args.role == e.role),
          args => employees.find(e => e.name == args.name),
          personAddress => Dao.personAddress(personAddress.fields.map(_.name)),
          personAddressPlan =>
            val cols = personAddressPlan.fields.flatMap(_.fields.map(_.name))
            println(s"==== Selected Columns: ${cols}")
            //println(s"==== Nested Fields: ${personAddressPlan.fields.map(_.fields.map(_.name))}")
            (Dao.personAddressPlan(cols) zip Dao.personAddress(cols)).map(
              (pa, plan) => PersonAddressPlanQuery(pa, plan)
            )
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
