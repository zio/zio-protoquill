package io.getquill.context.jdbc.sqlserver

import io.getquill.context.sql.BatchValuesSpec
import io.getquill._

class BatchValuesJdbcSpec extends BatchValuesSpec { //

  val context = testContext
  import testContext._

  override def beforeEach(): Unit = {
    testContext.run(sql"TRUNCATE TABLE Product; DBCC CHECKIDENT ('Product', RESEED, 1)".as[Delete[Product]])
    super.beforeEach()
  }

  "Ex 1 - Batch Insert Normal" in {
    import `Ex 1 - Batch Insert Normal`._
    inline def splicedOp = opExt(insert => sql"SET IDENTITY_INSERT Product ON; ${insert}".as[Insert[Product]])
    testContext.run(splicedOp, batchSize)
    testContext.run(get).toSet mustEqual result.toSet
  }

  "Ex 2 - Batch Insert Returning" in {
    import `Ex 2 - Batch Insert Returning`._
    val ids = testContext.run(op, batchSize)
    ids mustEqual expectedIds
    testContext.run(get) mustEqual result
  }

  "Ex 2B - Batch Insert Returning - whole row" in {
    import `Ex 2 - Batch Insert Returning`._
    inline given InsertMeta[Product] = insertMeta(_.id)
    inline def op = quote {
      liftQuery(products).foreach(p => query[Product].insertValue(p).returning(p => p))
    }
    val ids = testContext.run(op, batchSize)
    ids mustEqual productsOriginal
    testContext.run(get) mustEqual productsOriginal
  }

  "Ex 3 - Batch Insert Mixed" in {
    import `Ex 3 - Batch Insert Mixed`._
    inline def splicedOp = opExt(insert => sql"SET IDENTITY_INSERT Product ON; ${insert}".as[Insert[Product]])
    testContext.run(splicedOp, batchSize)
    testContext.run(get) mustEqual result
  }
}
