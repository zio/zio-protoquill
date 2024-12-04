package io.getquill.context.jdbc.postgres

import io.getquill.context.sql.BatchValuesSpec
import io.getquill.*


class BatchValuesJdbcSpecEncoders extends BatchValuesSpec with PostgresJdbcContext.Codec {
  type Session = java.sql.Connection
  type ResultRow = java.sql.ResultSet
  type PrepareRow = java.sql.PreparedStatement

  val context = testContext
  import testContext._

  override def beforeEach(): Unit = {
    testContext.run(sql"TRUNCATE TABLE Product RESTART IDENTITY CASCADE".as[Delete[Product]])
    super.beforeEach()
  }

  "Ex 1 - Batch Insert Normal" in {
    import `Ex 1 - Batch Insert Normal`._
    testContext.run(op, batchSize)
    testContext.run(get) mustEqual result
  }

  "Ex 2 - Batch Insert Returning" in {
    import `Ex 2 - Batch Insert Returning`._
    val ids = testContext.run(op, batchSize)
    ids mustEqual expectedIds
    testContext.run(get) mustEqual result
  }

  "Ex 3 - Batch Insert Mixed" in {
    import `Ex 3 - Batch Insert Mixed`._
    testContext.run(op, batchSize)
    testContext.run(get) mustEqual result
  }
}