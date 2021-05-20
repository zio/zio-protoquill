package io.getquill.context.sql

import io.getquill.Ord
import io.getquill._

trait QueryResultTypeSpec extends ProductSpec {

  import context._

  inline def deleteAll = quote(query[Product].delete)
  inline def selectAll = quote(query[Product])
  inline def map = quote(query[Product].map(_.id))
  inline def filter = quote(query[Product].filter(_ => true))
  inline def withFilter = quote(query[Product].withFilter(_ => true))
  inline def sortBy = quote(query[Product].sortBy(_.id)(Ord.asc))
  inline def take = quote(query[Product].take(10))
  inline def drop = quote(query[Product].drop(1))
  inline def `++` = quote(query[Product] ++ query[Product])
  inline def unionAll = quote(query[Product].unionAll(query[Product]))
  inline def union = quote(query[Product].union(query[Product]))

  inline def minExists = quote(query[Product].map(_.sku).min)
  inline def minNonExists = quote(query[Product].filter(_.id > 1000).map(_.sku).min)
  inline def maxExists = quote(query[Product].map(_.sku).max)
  inline def maxNonExists = quote(query[Product].filter(_.id > 1000).map(_.sku).max)
  inline def avgExists = quote(query[Product].map(_.sku).avg)
  inline def avgNonExists = quote(query[Product].filter(_.id > 1000).map(_.sku).avg)
  inline def productSize = quote(query[Product].size)
  inline def parametrizedSize = quote { (id: Long) =>
    query[Product].filter(_.id == id).size
  }

  inline def join = quote(query[Product].join(query[Product]).on(_.id == _.id))

  inline def nonEmpty = quote(query[Product].nonEmpty)
  inline def isEmpty = quote(query[Product].isEmpty)
  inline def distinct = quote(query[Product].map(_.id).distinct)

}
