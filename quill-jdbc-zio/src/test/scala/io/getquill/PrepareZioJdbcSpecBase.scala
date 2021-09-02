package io.getquill

import io.getquill.ZioTestUtil._
import io.getquill.context.ZioJdbc._
import io.getquill.context.jdbc.ResultSetExtractor
import io.getquill.context.sql.ProductSpec
import io.getquill.context.qzio.ZioJdbcContext
import org.scalactic.Equality
import zio.{Task, ZIO}
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType.Generic

import java.sql.{PreparedStatement, ResultSet}
import _root_.io.getquill.context.jdbc.JdbcContext

trait PrepareZioJdbcSpecBase extends ProductSpec with ZioSpec {

  val context: ZioJdbcContext[_, _]
  import context._

  implicit val productEq: Equality[Product] = new Equality[Product] {
    override def areEqual(a: Product, b: Any): Boolean = b match {
      case Product(_, desc, sku) => desc == a.description && sku == a.sku
      case _                     => false
    }
  }

  def productExtractor = (rs: ResultSet, session: Session) => summon[GenericDecoder[context.ResultRow, context.Session, Product, Generic]](0, rs, session)

  def withOrderedIds(products: List[Product]) =
    products.zipWithIndex.map { case (product, id) => product.copy(id = id.toLong + 1) }

  def singleInsert(prep: QIO[PreparedStatement]) = {
    prep.flatMap(stmt =>
      Task(stmt).bracketAuto { stmt => Task(stmt.execute()) }
    ).provideConnectionFrom(pool).defaultRun
  }

  def batchInsert(prep: QIO[List[PreparedStatement]]) = {
    prep.flatMap(stmts =>
      ZIO.collectAll(
        stmts.map(stmt =>
          Task(stmt).bracketAuto { stmt => Task(stmt.execute()) }
        )
      )
    ).provideConnectionFrom(pool).defaultRun
  }

  def extractResults[T](prep: QIO[PreparedStatement])(extractor: (ResultSet, Session) => T) = {
    (for {
      bconn <- ZIO.environment[QConnection]
      conn = bconn.get[java.sql.Connection]
      out <-
        prep.bracketAuto { stmt =>
          Task(stmt.executeQuery()).bracketAuto { rs =>
            Task(ResultSetExtractor(rs, conn, extractor))
          }
        }.provide(bconn)
    } yield (out)).provideConnectionFrom(pool).defaultRun
  }

  def extractProducts(prep: QIO[PreparedStatement]) =
    extractResults(prep)(productExtractor)
}
