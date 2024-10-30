package io.getquill.postgres

import io.getquill.*
import io.getquill.ast.{Filter, ReturningGenerated}
import io.getquill.context.AstSplicing
import io.getquill.context.sql.ProductSpec

class DiagnosticDataSpec extends ProductSpec with ZioSpec {

  val context = testContextSplicing
  import testContextSplicing.*

  override def beforeAll() = {
    super.beforeAll()
    testContextSplicing.run(quote(query[Product].delete)).runSyncUnsafe()
    ()
  }

  "Diagnostic data" - {
    "Should expose last executed query" in {
      val (insertQuery, selectQuery, insertInfo, selectInfo) = (for {
        _ <- testContextSplicing.run {
          liftQuery(productEntries).foreach(e => productInsert(e))
        }
        insertQuery <- getLastExecutedQuery()
        insertInfo <- getLastExecutionInfo()

        _ <- testContextSplicing.run(query[Product].filter(p => p.description == "Notebook"))
        selectQuery <- getLastExecutedQuery()
        selectInfo <- getLastExecutionInfo()

      } yield (insertQuery, selectQuery, insertInfo, selectInfo)).runSyncUnsafe()

      insertQuery.get mustBe "INSERT INTO Product (description,sku) VALUES (?, ?) RETURNING id"
      selectQuery.get mustBe "SELECT p.id, p.description, p.sku FROM Product p WHERE p.description = 'Notebook'"

      insertInfo.get.ast mustBe a[ReturningGenerated]
      selectInfo.get.ast mustBe a[Filter]
    }
  }
}
