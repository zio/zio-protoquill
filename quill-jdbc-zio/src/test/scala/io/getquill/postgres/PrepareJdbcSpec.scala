package io.getquill.postgres

import io.getquill.Prefix
import io.getquill.{ PrepareZioJdbcSpecBase, ZioSpec }
import org.scalatest.BeforeAndAfter
import io.getquill._

import java.sql.{ Connection, ResultSet }

class PrepareJdbcSpec extends PrepareZioJdbcSpecBase with ZioSpec with BeforeAndAfter {

  override def prefix: Prefix = Prefix("testPostgresDB")
  val context: testContext.type = testContext
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
