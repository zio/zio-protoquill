package io.getquill.context.jdbc.postgres

import io.getquill.context.sql.BatchValuesSpec
import io.getquill.*
import io.getquill.context.jdbc.JdbcSpecEncoders

class BatchValuesJdbcSpecEncoders extends BatchValuesSpec with JdbcSpecEncoders {

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