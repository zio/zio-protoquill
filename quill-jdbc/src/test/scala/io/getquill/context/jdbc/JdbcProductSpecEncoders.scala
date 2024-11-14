package io.getquill.context.jdbc

import io.getquill.context.sql.ProductSpec

trait JdbcProductSpecEncoders extends ProductSpec with JdbcSpecEncoders {
  given JdbcContext.GenericDecoder[Product] = JdbcContext.deriveDecoder
}
