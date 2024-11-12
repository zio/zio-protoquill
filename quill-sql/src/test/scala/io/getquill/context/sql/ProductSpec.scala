package io.getquill.context.sql

import io.getquill.Spec
import io.getquill.Query
import io.getquill.context.Context
import io.getquill._
import io.getquill.generic.{DecodingType, GenericDecoder}

case class Id(value: Long) extends AnyVal

trait ProductSpec extends Spec {
  type SpecSession
  type SpecPrepareRow
  type SpecResultRow

  val context: Context[_, _] {
    type Session = SpecSession
    type PrepareRow = SpecPrepareRow
    type ResultRow = SpecResultRow
  }
  import context._

  case class Product(id: Long, description: String, sku: Long)
  given productDecoder: GenericDecoder[SpecResultRow, SpecSession, Product, DecodingType.Generic]

  inline def product = quote {
    query[Product]
  }

  inline def productInsert = quote {
    (p: Product) => query[Product].insertValue(p).returningGenerated(_.id)
  }

  inline def productInsertBatch = quote {
    (b: Query[Product]) => b.foreach(p => productInsert.apply(p))
  }

  inline def productById = quote {
    (id: Long) => product.filter(_.id == id)
  }

  val productEntries = List(
    Product(0L, "Notebook", 1001L),
    Product(0L, "Soap", 1002L),
    Product(0L, "Pencil", 1003L)
  )

  inline def productSingleInsert = quote {
    product.insert(_.id -> 0, _.description -> "Window", _.sku -> 1004L).returningGenerated(_.id)
  }
}
