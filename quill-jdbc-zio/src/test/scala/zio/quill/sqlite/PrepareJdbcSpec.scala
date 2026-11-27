package zio.quill.sqlite

import zio.quill.context.qzio.ImplicitSyntax.Implicit
import javax.sql.DataSource
import java.sql.{ Connection, ResultSet }
import zio.quill.PrepareZioJdbcSpecBase

import org.scalatest.BeforeAndAfter
import zio.quill._

class PrepareJdbcSpec extends PrepareZioJdbcSpecBase with BeforeAndAfter {

  implicit val ds: Implicit[DataSource] = Implicit(pool)
  val context: testContext.underlying.type = testContext.underlying
  import testContext.underlying._

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
