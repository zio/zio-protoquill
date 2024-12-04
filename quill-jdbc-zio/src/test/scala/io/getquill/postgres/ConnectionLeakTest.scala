package io.getquill.postgres

import java.util.UUID
import io.getquill._
import io.getquill.context.sql.ProductSpec
import io.getquill.util.LoadConfig
import io.getquill.context.ZioJdbc._
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.{ Runtime, Unsafe }
import io.getquill.jdbczio.Quill

import scala.util.Random
import zio.ZLayer
import javax.sql.DataSource

class ConnectionLeakTest extends ProductSpec with ZioSpec with PostgresJdbcContext.Codec {

  implicit val pool: Implicit[ZLayer[Any, Throwable, DataSource]] = Implicit(Quill.DataSource.fromPrefix("testPostgresLeakDB"))

  // Only used for connection-amount checking
  val dataSource = JdbcContextConfig(LoadConfig("testPostgresLeakDB")).dataSource

  val context: PostgresZioJdbcContext[Literal] = new PostgresZioJdbcContext(Literal)
  import context._

  override def beforeAll() = {
    super.beforeAll()
    context.run(quote(query[Product].delete)).provide(pool.env).runSyncUnsafe()
    ()
  }

  "insert and select without leaking" in {
    val result =
      Unsafe.unsafe { implicit unsafe =>
        Runtime.default.unsafe.run(context.underlying.transaction {
          import context.underlying._
          for {
            _ <- context.underlying.run {
              quote {
                query[Product].insertValue(
                  lift(Product(1, UUID.randomUUID().toString, Random.nextLong()))
                )
              }
            }
            result <- context.underlying.run {
              query[Product].filter(p => query[Product].map(_.id).max.exists(_ == p.id))
            }
          } yield (result)
        }
          .map(_.headOption.map(_.id))
          .onDataSource
          .provide(pool.env))
          .getOrThrow()
      }

    Thread.sleep(2000)

    result mustEqual Option(1)
    dataSource.getHikariPoolMXBean.getActiveConnections mustEqual 0

    context.close()
  }

}
