package io.getquill.context.sql

import io.getquill.*
import org.scalatest.BeforeAndAfterEach
import io.getquill.generic.{DecodingType, GenericDecoder}



trait BatchValuesSpec extends Spec with BeforeAndAfterEach {
  type SpecSession
  type SpecPrepareRow
  type SpecResultRow

  val context: SqlContext[_, _] {
    type Session = SpecSession
    type PrepareRow = SpecPrepareRow
    type ResultRow = SpecResultRow
  }
  import context._

  case class Product(id: Int, description: String, sku: Long)
  given productDecoder: GenericDecoder[SpecResultRow, SpecSession, Product, DecodingType.Composite]

  inline def insertProduct =
    quote((p: Product) => query[Product].insertValue(p))

  def makeProducts(maxRows: Int) =
    (1 to 22).map(i => Product(i, s"Product-${i}", i * 100))

  object `Ex 1 - Batch Insert Normal` {
    inline given InsertMeta[Product] = insertMeta(_.id)
    val products = makeProducts(22)
    val batchSize = 5
    inline def opExt = quote {
      (transform: Insert[Product] => Insert[Product]) =>
        liftQuery(products).foreach(p => transform(query[Product].insertValue(p)))
    }
    inline def op = quote {
      liftQuery(products).foreach(p => query[Product].insertValue(p))
    }
    inline def get = quote { query[Product] }
    def result = products
  }

  object `Ex 2 - Batch Insert Returning` {
    val productsOriginal = makeProducts(20)
    // want to populate them from DB
    val products = productsOriginal.map(p => p.copy(id = 0))
    val expectedIds = productsOriginal.map(_.id)
    val batchSize = 10
    inline def op = quote {
      liftQuery(products).foreach(p => query[Product].insertValue(p).returningGenerated(p => p.id))
    }
    inline def get = quote { query[Product] }
    def result = productsOriginal
  }

  object `Ex 3 - Batch Insert Mixed` {
    val products = makeProducts(20)
    val batchSize = 40
    inline def op = quote {
      liftQuery(products).foreach(p => query[Product].insert(_.id -> p.id, _.description -> lift("BlahBlah"), _.sku -> p.sku))
    }
    inline def opExt = quote {
      (transform: Insert[Product] => Insert[Product]) =>
        liftQuery(products).foreach(p => transform(query[Product].insert(_.id -> p.id, _.description -> lift("BlahBlah"), _.sku -> p.sku)))
    }
    inline def get = quote { query[Product] }
    def result = products.map(_.copy(description = "BlahBlah"))
  }
}
