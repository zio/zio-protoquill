package io.getquill.context.jdbc.postgres

import io.getquill.context.encoding.OptionalNestedSpec
import io.getquill._

class OptionalProductEncodingJdbcSpec extends OptionalNestedSpec {

  val context: testContext.type = testContext
  import testContext._

  override protected def beforeEach() = {
    import Setup._
    testContext.run(query[Contact].delete)
    ()
  }

  "2.Optional Inner Product" - {

    import `2.Optional Inner Product with Optional Leaf`._
    "2.Ex3 Auto - Null inner leaf" in {
      val result = `2.Ex3 - Null inner leaf result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }
  }

}
