package io.getquill.context.jdbc.oracle

import java.sql.ResultSet

import io.getquill.context.jdbc.PrepareJdbcSpecEncodersBase
import org.scalatest.BeforeAndAfter
import io.getquill._

class PrepareJdbcSpecEncoders extends PrepareJdbcSpecEncodersBase with OracleJdbcContext.Codec with BeforeAndAfter {

  val context: testContext.type = testContext
  import testContext._

  before {
    testContext.run(query[Product].delete)
  }

  val prepareQuery = prepare(query[Product])

  "single" in {
    val prepareInsert = prepare(query[Product].insertValue(lift(productEntries.head)))
    singleInsert(dataSource.getConnection)(prepareInsert) mustEqual false
    extractProducts(dataSource.getConnection)(prepareQuery) === List(productEntries.head)
  }

  "batch" in {
    val prepareBatchInsert = prepare(
      liftQuery(withOrderedIds(productEntries)).foreach(p => query[Product].insertValue(p))
    )

    batchInsert(dataSource.getConnection)(prepareBatchInsert).distinct mustEqual List(false)
    extractProducts(dataSource.getConnection)(prepareQuery) === withOrderedIds(productEntries)
  }
}
