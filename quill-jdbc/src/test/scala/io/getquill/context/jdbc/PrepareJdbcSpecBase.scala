package io.getquill.context.jdbc

import java.sql.{Connection, PreparedStatement, ResultSet}
import io.getquill.context.sql.ProductSpec
import io.getquill.util.Using.Manager
import org.scalactic.Equality

import scala.util.{Failure, Success}
import io.getquill.generic.{DecodingType, GenericDecoder}
import io.getquill.generic.DecodingType.Generic

trait JdbcContextSpec {
  given nullChecker: JdbcNullChecker = new JdbcNullChecker()
  given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific]
  given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific]
  given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific]
  given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific]
}

trait JdbcProductSpec extends JdbcContextSpec {
  given JdbcContext.GenericDecoder[Product] = JdbcContext.deriveDecoder
}

trait PrepareJdbcSpecBase extends JdbcContextSpec {

  val context: JdbcContext[_, _]
  import context._

  implicit val productEq: Equality[Product] = new Equality[Product] {
    override def areEqual(a: Product, b: Any): Boolean = b match {
      case Product(_, desc, sku) => desc == a.description && sku == a.sku
      case _                     => false
    }
  }

  def productExtractor =
    (rs: ResultSet, s: Session) =>
      summon[GenericDecoder[context.ResultRow, context.Session, Product, Generic]](0, rs, s)

  def withOrderedIds(products: List[Product]) =
    products.zipWithIndex.map { case (product, id) => product.copy(id = id.toLong + 1) }

  def singleInsert(conn: => Connection)(prep: Connection => PreparedStatement) = {
    val flag = Manager { use =>
      val c = use(conn)
      val s = use(prep(c))
      s.execute()
    }
    flag match {
      case Success(value) => value
      case Failure(e)     => throw e
    }
  }

  def batchInsert(conn: => Connection)(prep: Connection => List[PreparedStatement]) = {
    val r = Manager { use =>
      val c = use(conn)
      val st = prep(c)
      appendExecuteSequence(st)
    }
    r.flatten match {
      case Success(value) => value
      case Failure(e)     => throw e
    }
  }

  def extractResults[T](conn: => Connection)(prep: Connection => PreparedStatement)(extractor: (ResultSet, Connection) => T) = {
    val r = Manager { use =>
      val c = use(conn)
      val st = use(prep(c))
      val rs = st.executeQuery()
      ResultSetExtractor(rs, c, extractor)
    }
    r match {
      case Success(v) => v
      case Failure(e) => throw e
    }
  }

  def extractProducts(conn: => Connection)(prep: Connection => PreparedStatement): List[Product] =
    extractResults(conn)(prep)(productExtractor)

  def appendExecuteSequence(actions: => List[PreparedStatement]) = {
    Manager { use =>
      actions.map { stmt =>
        val s = use(stmt)
        s.execute()
      }
    }
  }
}
