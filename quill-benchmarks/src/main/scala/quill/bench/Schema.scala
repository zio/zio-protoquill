package quill.bench

import java.time.LocalDateTime
import zio.quill.*
import zio.quill.context.{SplicingBehavior, SplicingBehaviorHint}

object Schema {
  case class CustomerId(value: Long)
  case class Customer(
    id: CustomerId, name: String, email: Option[String], country: String,
    active: Boolean, createdAt: LocalDateTime
  )
}

abstract class QueryFixture(val ctx: PostgresJdbcContext[SnakeCase]) {
  import ctx.*
  import Schema.*
  given MappedEncoding[CustomerId, Long] = MappedEncoding(_.value)
  given MappedEncoding[Long, CustomerId] = MappedEncoding(CustomerId.apply)
  // Dynamic fallback would move Quill work out of compilation and invalidate the benchmark.
  given SplicingBehaviorHint with { type BehaviorType = SplicingBehavior.FailOnDynamic }
  inline def customers = quote(querySchema[Customer]("customer"))
}
