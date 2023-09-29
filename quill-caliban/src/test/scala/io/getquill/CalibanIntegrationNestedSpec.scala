package io.getquill

import zio.{ZIO, Task}
import io.getquill.context.ZioJdbc._
import caliban.execution.Field
import caliban.schema.ArgBuilder
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.RootResolver
import io.getquill.CalibanIntegration._

class CalibanIntegrationNestedSpec extends CalibanSpec {
  import Ctx._

  object Nested {
    import NestedSchema._
    object Dao {
      def personAddress(columns: List[String], filters: Map[String, String]) =
        Ctx.run {
          query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
            .map((p, a) => PersonAddressNested(p.id, p.name, p.age, a.map(_.street)))
            .filterByKeys(filters)
            .filterColumns(columns)
            .take(10)
        }.provideLayer(zioDS).tap(list => {
          println(s"Results: $list for columns: $columns and filters: ${io.getquill.util.Messages.qprint(filters)}")
          ZIO.unit
        })
        .tapError(e => {
          println(s"ERROR $e")
          ZIO.unit
        })
    }
  }

  case class Queries(
    personAddressNested: Field => (ProductArgs[NestedSchema.PersonAddressNested] => Task[List[NestedSchema.PersonAddressNested]])
  )

  val api = graphQL(
      RootResolver(
        Queries(
          personAddressNested =>
            (productArgs =>
              Nested.Dao.personAddress(quillColumns(personAddressNested), productArgs.keyValues)
            ),
        )
      )
    )

  "Caliban integration should work for nested object" - {
    "with no top-level filter" in {
      val query =
        """
        {
          personAddressNested {
            id
          }
        }"""
      unsafeRunQuery(query) mustEqual """{"personAddressNested":[{"id":1},{"id":2},{"id":3}]}"""
    }

    "top-level filter is nested field" in {
      val query =
        """
        {
          personAddressNested(name: { first: "One" }) {
            id
            name {
              first
            }
          }
        }"""
      val output = unsafeRunQuery(query)
      println("========== QUERY OUTPUT: " + output)
      output mustEqual """{"personAddressNested":[{"id":1,"name":{"first":"One"}}]}"""
    }

    "top-level filter is nested field and does not occur in body" in {
      val query =
        """
        {
          personAddressNested(name: { first: "One" }) {
            id
          }
        }"""
      unsafeRunQuery(query) mustEqual """{"personAddressNested":[{"id":1}]}"""
    }

    "with one field in the nested object" in {
      val query =
        """
        {
          personAddressNested(id: 1) {
            id
            age
            name {
              first
            }
          }
        }"""
      unsafeRunQuery(query) mustEqual """{"personAddressNested":[{"id":1,"age":44,"name":{"first":"One"}}]}"""
    }

    "with multiple fields in the nested object" in {
      val query =
        """
        {
          personAddressNested(id: 1) {
            id
            age
            name {
              first
              last
            }
          }
        }"""
      unsafeRunQuery(query) mustEqual """{"personAddressNested":[{"id":1,"age":44,"name":{"first":"One","last":"A"}}]}"""
    }
  }
}