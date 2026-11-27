package zio.quill.context.cassandra.zio

import _root_.zio.quill.context.cassandra.QueryResultTypeCassandraSpec
import _root_.zio.quill._

class QueryResultTypeCassandraZioSpec extends ZioCassandraSpec with QueryResultTypeCassandraSpec {

  val context: testZioDB.type = testZioDB

  import context._

  inline def orders = quote(querySchema[OrderTestEntity]("OrderTestEntityZio"))

  override def beforeAll() = {
    super.beforeAll()
    result(context.run(deleteAll))
    result(context.run(liftQuery(entries).foreach(e => insert(e))))
    ()
  }

  "query" in {
    result(context.run(selectAll)) mustEqual entries
  }

  "stream" in {
    result(context.stream(selectAll)) mustEqual entries
  }

  "querySingle" - {
    "size" in {
      result(context.run(entitySize)) mustEqual 3
    }
    "parametrized size" in {
      result(context.run(parametrizedSize(lift(10000)))) mustEqual 0
    }
  }
}
