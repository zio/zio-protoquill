package io.getquill.sqlserver

import java.sql.{ Connection, ResultSet }
import io.getquill.PrepareZioJdbcSpecBase
import io.getquill.Prefix
import org.scalatest.BeforeAndAfter
import io.getquill._

class PrepareJdbcSpec extends PrepareZioJdbcSpecBase with BeforeAndAfter {

  override def prefix: Prefix = Prefix("testSqlServerDB")
  val context: testContext.type = testContext
  import testContext._

  before {
    testContext.run(query[Product].delete).runSyncUnsafe()
  }

  val prepareQuery = prepare(query[Product])
  // TODO Try removing 'inline' and do the old implicit way and see if an error results
    inline given InsertMeta[Product] = insertMeta[Product](_.id)

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
