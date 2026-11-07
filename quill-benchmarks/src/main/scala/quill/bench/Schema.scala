package quill.bench

import java.time.LocalDateTime
import zio.quill.*
import zio.quill.context.{SplicingBehavior, SplicingBehaviorHint}

object Schema {
  object CustomerId {
    opaque type Type = Long
    def apply(value: Long): Type = value
    given MappedEncoding[Type, Long] = MappedEncoding(identity)
    given MappedEncoding[Long, Type] = MappedEncoding(identity)
  }
  type CustomerId = CustomerId.Type
  enum OrderStatus { case Draft, Open, Paid, Cancelled }
  object OrderStatus {
    given MappedEncoding[OrderStatus, String] = MappedEncoding(_.toString)
    given MappedEncoding[String, OrderStatus] = MappedEncoding(OrderStatus.valueOf)
  }
  case class Customer(
    id: CustomerId, name: String, email: Option[String], country: String,
    active: Boolean, createdAt: LocalDateTime
  )
  case class PurchaseOrder(
    id: Long,
    customerId: CustomerId,
    placedAt: LocalDateTime,
    status: OrderStatus,
    total: BigDecimal,
    currency: String,
    discount: Option[BigDecimal],
    note: Option[String],
    shipName: String,
    shipStreet: String,
    shipCity: String,
    shipPostalCode: String,
    shipCountry: String,
    paidAt: Option[LocalDateTime],
    cancelledAt: Option[LocalDateTime],
    version: Int
  )
  case class OrderLine(orderId: Long, productId: Long, quantity: Int, unitPrice: BigDecimal, total: BigDecimal)
  case class Country(code: String)
  case class Shipment(id: Long, orderId: Long, recipientId: Option[CustomerId], carrier: String, trackingCode: Option[String])
  case class Payment(id: Long, orderId: Long, amount: BigDecimal, receivedAt: LocalDateTime, reference: Option[String])
  case class OrderState(orderId: Long, changedAt: LocalDateTime, status: OrderStatus, changedBy: Option[CustomerId])
  case class RevenueSummary(customerId: CustomerId, currency: String, net: BigDecimal, statuses: List[OrderStatus])
  case class Stock(productId: Long, warehouseId: Long, quantity: Int, updatedAt: LocalDateTime)
}

abstract class QueryFixture(val ctx: PostgresJdbcContext[SnakeCase]) {
  import ctx.*
  import Schema.*
  // Dynamic fallback would move Quill work out of compilation and invalidate the benchmark.
  given SplicingBehaviorHint with { type BehaviorType = SplicingBehavior.FailOnDynamic }
  inline def customers = quote(querySchema[Customer]("customer"))
  inline def orders = quote(querySchema[PurchaseOrder]("purchase_order"))
  inline def lines = quote(querySchema[OrderLine]("order_line"))
  inline def shipments = quote(querySchema[Shipment]("shipment"))
  inline def payments = quote(querySchema[Payment]("payment"))
  inline def states = quote(querySchema[OrderState]("order_state"))
  inline def stock = quote(querySchema[Stock]("stock"))
  inline def openOrders = quote(orders.filter(o => o.status == lift(OrderStatus.Open) && o.cancelledAt.isEmpty))
}
