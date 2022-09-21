package io.getquill

import zio.{ZIO, Task}
import io.getquill.context.ZioJdbc._
import caliban.execution.Field
import caliban.schema.ArgBuilder
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.RootResolver
import io.getquill.CalibanIntegration._

class CalibanIntegrationSpec extends CalibanSpec {
  import Ctx._

  object Flat:
    import FlatSchema._
    object Dao:
      def personAddress(columns: List[String], filters: Map[String, String]): ZIO[Any, Throwable, List[PersonAddress]] =
        Ctx.run {
          query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
            .map((p, a) => PersonAddress(p.id, p.first, p.last, p.age, a.map(_.street)))
          .filterByKeys(filters)
            //.filterColumns(columns) // //
            .take(10)
        }.provideLayer(zioDS).tap(list => {
          println(s"Results: $list for columns: $columns")
          ZIO.unit
        })

  case class Queries(
    personAddressFlat: Field => (ProductArgs[FlatSchema.PersonAddress] => Task[List[FlatSchema.PersonAddress]]),
  )

  val api = graphQL(
      RootResolver(
        Queries(
          personAddressFlat =>
            (productArgs =>
              Flat.Dao.personAddress(quillColumns(personAddressFlat), productArgs.keyValues)
            )
        )
      )
    )

  "Caliban integration should work for flat object" - {
    "with no filters" in {
      val query =
        """
        {
          personAddressFlat {
            id
          }
        }"""
      unsafeRunQuery(query) mustEqual """{"personAddressFlat":[{"id":1},{"id":2},{"id":3}]}"""
    }

    "with filtration column included" in {
      val query =
        """
        {
          personAddressFlat(first: "One") {
            id
            first
            last
            street
          }
        }"""
      unsafeRunQuery(query) mustEqual """{"personAddressFlat":[{"id":1,"first":"One","last":"A","street":"123 St"}]}"""
    }
    "with no filtration column excluded" in {
      val query =
        """
        {
          personAddressFlat(first: "One") {
            id
            last
            street
          }
        }"""
      unsafeRunQuery(query) mustEqual """{"personAddressFlat":[{"id":1,"last":"A","street":"123 St"}]}"""
    }
  }
}