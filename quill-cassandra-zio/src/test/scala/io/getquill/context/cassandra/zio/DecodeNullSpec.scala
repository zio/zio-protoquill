package io.getquill.context.cassandra.zio

import io.getquill._

class DecodeNullSpec extends ZioCassandraSpec {

  "no default values when reading null" - {
    "stream" in {
      import testZioDB._
      inline def writeEntities = quote(querySchema[DecodeNullTestWriteEntity]("DecodeNullTestEntity"))

      val ret =
        for {
          _ <- testZioDB.run(writeEntities.delete)
          _ <- testZioDB.run(writeEntities.insertValue(lift(insertee)))
          result <- testZioDB.run(query[DecodeNullTestEntity])
        } yield {
          result
        }

      result(ret.foldCause(
        cause => {
          cause.isDie must equal(true)
          cause.dieOption match {
            case Some(e: Exception) =>
              e.isInstanceOf[IllegalStateException] must equal(true)
            case _ =>
              fail("Expected Fatal Error to be here (and to be a IllegalStateException")
          }
        },
        success =>
          fail("Expected Exception IllegalStateException but operation succeeded")
      ))
      ()
    }
  }

  case class DecodeNullTestEntity(id: Int, value: Int)

  case class DecodeNullTestWriteEntity(id: Int, value: Option[Int])

  val insertee = DecodeNullTestWriteEntity(0, None)
}
