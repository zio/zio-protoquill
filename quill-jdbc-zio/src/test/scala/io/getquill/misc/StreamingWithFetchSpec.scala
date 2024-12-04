package io.getquill.misc

import io.getquill.ZioSpec
import io.getquill.context.ZioJdbc._
import org.scalatest.BeforeAndAfter
import io.getquill._
import zio.Unsafe
import zio.ZEnvironment

class StreamingWithFetchSpec extends ZioProxySpec with BeforeAndAfter with PostgresJdbcContext.Codec {

  val context = testContext
  import testContext._

  case class Person(name: String, age: Int)
  given CompositeDecoder[Person] = deriveComposite

  inline def selectAll = quote(query[Person])
  inline def insert = quote { (p: Person) => query[Person].insertValue(p) }

  def result[T](qzio: QIO[T]): T =
    Unsafe.unsafe { implicit unsafe =>
      qzio.provideEnvironment(ZEnvironment(io.getquill.postgres.pool)).runSyncUnsafe()
    }

  before {
    testContext.run(quote(query[Person].delete)).runSyncUnsafe()
    ()
  }
  "streaming with fetch should work" - {
    def produceEntities(num: Int) =
      (1 to num).map(i => Person("Joe" + i, i)).toList

    "with entities == 1/2 * fetch size" in {
      val entities = produceEntities(5)
      result(
        context.run(liftQuery(entities).foreach(e => insert(e))) *>
          context.stream(selectAll, 10).runCollect.map(_.toList)
      ) must contain theSameElementsAs entities
    }
    "with entities == fetch size" in {
      val entities = produceEntities(10)
      result(
        context.run(liftQuery(entities).foreach(e => insert(e))) *>
          context.stream(selectAll, 10).runCollect.map(_.toList)
      ) must contain theSameElementsAs entities
    }
    "with entities == 1.5 * fetch size" in {
      val entities = produceEntities(15)
      result(
        context.run(liftQuery(entities).foreach(e => insert(e))) *>
          context.stream(selectAll, 10).runCollect.map(_.toList)
      ) must contain theSameElementsAs entities
    }
  }
}
