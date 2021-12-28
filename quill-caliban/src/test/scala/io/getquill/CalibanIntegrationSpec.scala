package io.getquill

import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import io.getquill.context.ZioJdbc.DataSourceLayer
import zio.{ZIO, Task}
import io.getquill.context.ZioJdbc._
import caliban.execution.Field
import caliban.schema.ArgBuilder
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.RootResolver
import io.getquill.CalibanIntegration._

class CalibanIntegrationSpec extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val zioDS = DataSourceLayer.fromPrefix("testPostgresDB")

  extension [R, E, A](qzio: ZIO[Any, Throwable, A])
    def unsafeRunSync(): A = zio.Runtime.default.unsafeRun(qzio)

  override def beforeAll() = {
    import Flat._
    (for {
        _ <- Ctx.run(infix"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
        _ <- Ctx.run(liftQuery(List(
          PersonT(1, "One", "A", 44),
          PersonT(2, "Two", "B", 55),
          PersonT(3, "Three", "C", 66)
        )).foreach(row => query[PersonT].insert(row)))
        _ <- Ctx.run(liftQuery(List(
          AddressT(1, "123 St"),
          AddressT(2, "789 St")
        )).foreach(row => query[AddressT].insert(row)))
      } yield ()
    ).onDataSource.provideLayer(zioDS).unsafeRunSync()
  }

  override def afterAll() = {
    import Flat._
    Ctx.run(infix"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]]).onDataSource.provideLayer(zioDS).unsafeRunSync()
  }

  object Flat:
    case class PersonT(id: Int, first: String, last: String, age: Int)
    case class AddressT(ownerId: Int, street: String)
    case class PersonAddressFlat(id: Int, first: String, last: String, age: Int, street: Option[String])
    object Dao:
      def personAddress(columns: List[String], filters: Map[String, String]) =
        Ctx.run {
          query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
            .map((p, a) => PersonAddressFlat(p.id, p.first, p.last, p.age, a.map(_.street)))
            .filterByKeys(filters)
            .filterColumns(columns)
            .take(10)
        }.onDataSource.provideLayer(zioDS)

  object Nested:
    case class Name(first: String, last: String)
    case class PersonT(id: Int, name: Name, age: Int)
    case class AddressT(ownerId: Int, street: String)
    // Needs to be named differently from Flat.PersonAddress___ since Caliban infers from this class & name must be different
    case class PersonAddressNested(id: Int, name: Name, age: Int, street: Option[String])
    object Dao:
      def personAddress(columns: List[String], filters: Map[String, String]) =
        Ctx.run {
          query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
            .map((p, a) => PersonAddressNested(p.id, p.name, p.age, a.map(_.street)))
            .filterByKeys(filters)
            .filterColumns(columns)
            .take(10)
        }.onDataSource.provideLayer(zioDS)

  case class Queries(
    personAddressFlat: Field => (ProductArgs[Flat.PersonAddressFlat] => Task[List[Flat.PersonAddressFlat]]),
    personAddressJoined: Field => (ProductArgs[Nested.PersonAddressNested] => Task[List[Nested.PersonAddressNested]])
  )

  val api = graphQL(
      RootResolver(
        Queries(
          personAddressFlat =>
            (productArgs =>
              Flat.Dao.personAddress(personAddressFlat.fields.map(_.name), productArgs.keyValues)
            ),
          personAddressNested =>
            (productArgs =>
              Nested.Dao.personAddress(personAddressNested.fields.map(_.name), productArgs.keyValues)
            ),
        )
      )
    )

  def unsafeRunQuery(queryString: String) =
    (for {
      interpreter <- api.interpreter
      result      <- interpreter.execute(queryString)
    } yield (result)).unsafeRunSync()

  object CalibanQueries:
    // Purposely filtering by a column that is not in the selection to test this case
    val flatWithJoin =
      """
      {
        personAddressFlat(first: One) {
          id
          first
          last
          street
        }
      }"""
    val flatWithJoinColNotIncluded =
      """
      {
        personAddressFlat(first: One) {
          id
          last
          street
        }
      }"""

  "Caliban integration should work for" - {
    "flat object with filteration column included" in {
      unsafeRunQuery(CalibanQueries.flatWithJoin).data.toString mustEqual """{"personAddressFlat":[{"id":1,"first":"One","last":"A","street":"123 St"}]}"""
    }
    "flat object with filteration column excluded" in {
      unsafeRunQuery(CalibanQueries.flatWithJoinColNotIncluded).data.toString mustEqual """{"personAddressFlat":[{"id":1,"last":"A","street":"123 St"}]}"""
    }
  }
}