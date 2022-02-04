package io.getquill.postgres

import java.util.UUID
import io.getquill.{ JdbcContextConfig, Literal, PostgresZioJdbcContext, ZioSpec }
import io.getquill.context.sql.ProductSpec

import io.getquill.util.LoadConfig
import io.getquill.context.ZioJdbc._
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime
import io.getquill._

import scala.util.Random
import zio.Runtime.Managed
import javax.sql.DataSource
import zio.ZIO
import zio.ZLayer
import com.zaxxer.hikari.HikariDataSource

class ConnectionLeakTest extends ProductSpec with ZioSpec {

  implicit val pool: Implicit[Managed[HikariDataSource]] =
    Implicit(zio.Runtime.unsafeFromLayer(ZLayer.succeed(JdbcContextConfig(LoadConfig("testPostgresLeakDB")).dataSource)))

  // Need to type this so that output of context.run(quote(query[Product].delete)) is correct
  // (also so that .runSyncUnsafe() extension method even can be added)
  val context: PostgresZioJdbcContext[Literal] = new PostgresZioJdbcContext(Literal)
  import context._

  override def beforeAll() = {
    super.beforeAll()
    context.run(quote(query[Product].delete)).runSyncUnsafe()
    ()
  }

  "insert and select without leaking" in {

    val execution =
      context.underlying.transaction {
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
          head <- ZIO.succeed(result.headOption.map(_.id))
        } yield (head)
      }
      .onDataSource

    val result = pool.env.unsafeRun(execution)

    Thread.sleep(2000)

    result mustEqual Option(1)
    val activeConnections = pool.env.unsafeRun(ZIO.service[HikariDataSource].map(_.getHikariPoolMXBean.getActiveConnections))
    activeConnections mustEqual 0

    context.close()
  }

}
