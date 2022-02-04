package io.getquill.mysql

import java.sql.{ Connection, ResultSet }
import io.getquill.PrepareZioJdbcSpecBase

import org.scalatest.BeforeAndAfter
import io.getquill._

class PrepareJdbcSpec extends PrepareZioJdbcSpecBase with BeforeAndAfter {

  val context = testContext
  import testContext._

  before {
    testContext.run(query[Product].delete).runSyncUnsafe()
  }

  val prepareQuery = prepare(query[Product])

  "single" in {
    val prepareInsert = prepare(query[Product].insertValue(lift(productEntries.head)))
    singleInsert(prepareInsert) mustEqual false
    extractProducts(prepareQuery) === List(productEntries.head)
  }

  "batch" in {
    val prepareBatchInsert = prepare(
      liftQuery(withOrderedIds(productEntries)).foreach(p => query[Product].insertValue(p))
    )

    batchInsert(prepareBatchInsert).distinct mustEqual List(false)
    extractProducts(prepareQuery) === withOrderedIds(productEntries)
  }
}
