package io.getquill

import io.getquill.context.sql.ProductSpec
import io.getquill.*
import io.getquill.context.mirror.MirrorSession
import io.getquill.context.mirror.Row

/** Some Quick tests to make sure action.returning works correctly */
class ReturningSpec extends MirrorSpec with ProductSpec {

  val context: SqlMirrorContext[PostgresDialect, Literal] = new SqlMirrorContext(PostgresDialect, Literal)
  import context._
  given productDecoder: MirrorContext.CompositeDecoder[Product] = MirrorContext.deriveComposite

  case class Foo(id: Long, description: String, sku: Long)
  given MirrorContext.CompositeDecoder[Foo] = MirrorContext.deriveComposite

  "postgres style" - {
    "returning - single insert with inlined free variable" in {
      val prd = Product(0L, "test1", 1L)
      val inserted = context.run {
        product.insert(_.sku -> lift(prd.sku), _.description -> lift(prd.description)).returning(r => r)
      }
      inserted.string mustEqual "INSERT INTO Product (sku,description) VALUES (?, ?) RETURNING id, description, sku"
    }
  }

  "sql server style" - {
    "returning - single insert with inlined free variable" - {
      val context: SqlMirrorContext[SQLServerDialect, Literal] = new SqlMirrorContext(SQLServerDialect, Literal)
      import context._
      val prd = Product(0L, "test1", 1L)
      val inserted = context.run {
        product.insert(_.sku -> lift(prd.sku), _.description -> lift(prd.description)).returning(r => r)
      }
      inserted.string mustEqual "INSERT INTO Product (sku,description) OUTPUT INSERTED.id, INSERTED.description, INSERTED.sku VALUES (?, ?)"
    }
  }

}
