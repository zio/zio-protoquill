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

  "2.Optional Inner Product" - { ////

    import `2.Optional Inner Product with Optional Leaf`._
    "2.Ex3 Auto - Null inner leaf" in {
      val result = `2.Ex3 - Null inner leaf result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }

    // [error] -- Error: /home/alexi/git/protoquill/quill-jdbc/src/test/scala/io/getquill/context/jdbc/postgres/OptionalProductEncodingJdbcSpec.scala:22:39
    // [error] 22 |      context.run(data.insertValue(lift(result)))
    // [error]    |                                   ^^^^^^^^^^^^
    // [error]    |The class Option[Age] (symbol: class Option) is not a case class in the expression: classShowable.map[Age](((prop: LastNameAge) => prop.age))
    // [error]    |Therefore you cannot lookup the property `age` on it!
    // [error]    | This location contains code that was inlined from OptionalProductEncodingJdbcSpec.scala:22

  }

}
