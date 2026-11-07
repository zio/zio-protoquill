package quill.bench.queries

import zio.quill.*
import zio.quill.extras.*
import quill.bench.{QueryFixture, Schema}
import Schema.*

class Revenue(context: PostgresJdbcContext[SnakeCase]) extends QueryFixture(context) {
  import ctx.*
  def revenue(since: java.time.LocalDateTime, minimum: BigDecimal) = run {
    orders.filter(o => o.placedAt >= lift(since) && o.status != lift(OrderStatus.Cancelled))
      .join(lines).on((o, l) => o.id == l.orderId)
      .map { case (o, l) =>
        val share = infix"${l.total} / NULLIF(SUM(${l.total}) OVER (PARTITION BY ${o.id}), 0)".as[BigDecimal]
        val net = if o.discount.isDefined then l.total - o.discount.getOrElse(lift(BigDecimal(0))) * share else l.total
        (o.customerId, o.currency, o.status, net)
      }.nested
      .groupBy(row => (row._1, row._2))
      .map { case ((customerId, currency), rows) =>
        RevenueSummary(customerId, currency, rows.map(_._4).sum.getOrElse(lift(BigDecimal(0))),
          infix"ARRAY_AGG(DISTINCT ${rows.map(_._3)})".as[List[OrderStatus]])
      }
      .filter(_.net >= lift(minimum))
      .sortBy(_.net)(Ord.desc)
  }
}
