package io.getquill.context.jdbc.postgres

import io.getquill.context.sql.ArrayOpsSpec
import io.getquill._
import io.getquill.generic.ArrayEncoding
import io.getquill.context.sql.SqlContext

class JdbcArrayOpsSpec extends ArrayOpsSpec with PostgresJdbcContext.Codec {
  // Need to cast the context to get PostgresDialect, Literal working otherwise it will try to summon 'Idiom'
  // maybe there should be a fallback for that actually
  val context: testContext.type = testContext //: SqlContext[PostgresDialect, Literal] with ArrayEncoding
  import context._

  "contains" in {
    context.run(`contains`.`Ex 1 return all`) mustBe `contains`.`Ex 1 expected`
    context.run(`contains`.`Ex 2 return 1`) mustBe `contains`.`Ex 2 expected`
    context.run(`contains`.`Ex 3 return 2,3`) mustBe `contains`.`Ex 3 expected`
    context.run(`contains`.`Ex 4 return empty`) mustBe `contains`.`Ex 4 expected`
  }

  override protected def beforeAll(): Unit = {
    context.run(entity.delete)
    context.run(insertEntries)
    ()
  }
}
