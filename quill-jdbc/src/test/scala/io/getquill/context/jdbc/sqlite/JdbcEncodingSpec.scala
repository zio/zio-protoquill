package io.getquill.context.jdbc.sqlite

import io.getquill.context.sql.EncodingSpec
import io.getquill._
import java.time.ZoneId
import io.getquill.context.jdbc.JdbcSpecEncoders

class JdbcEncodingSpec extends EncodingSpec with JdbcSpecEncoders {

  val context: testContext.type = testContext
  import testContext._

  "encodes and decodes types" in {
    testContext.run(delete)
    testContext.run(liftQuery(insertValues).foreach(e => insert(e)))
    verify(testContext.run(query[EncodingTestEntity]))
    ()
  }

  "Encode/Decode Other Time Types" in {
    context.run(query[TimeEntity].delete)
    val zid = ZoneId.systemDefault()
    val timeEntity = TimeEntity.make(zid)
    context.run(query[TimeEntity].insertValue(lift(timeEntity)))
    val actual = context.run(query[TimeEntity]).head
    timeEntity mustEqual actual
  }
}
