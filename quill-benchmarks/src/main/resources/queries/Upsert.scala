package quill.bench.queries

import zio.quill.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Upsert(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def sync(items: List[PurchaseOrder]) = run {
    liftQuery(items).foreach(item => orders.insertValue(item).onConflictUpdate(_.id)(
      (current, incoming) => current.customerId -> incoming.customerId,
      (current, incoming) => current.status -> incoming.status,
      (current, incoming) => current.total -> incoming.total,
      (current, incoming) => current.currency -> incoming.currency,
      (current, incoming) => current.discount -> incoming.discount,
      (current, incoming) => current.note -> incoming.note,
      (current, incoming) => current.shipName -> incoming.shipName,
      (current, incoming) => current.shipStreet -> incoming.shipStreet,
      (current, incoming) => current.shipCity -> incoming.shipCity,
      (current, incoming) => current.shipPostalCode -> incoming.shipPostalCode,
      (current, incoming) => current.shipCountry -> incoming.shipCountry,
      (current, incoming) => current.version -> (current.version + 1)
    ))
  }
}
