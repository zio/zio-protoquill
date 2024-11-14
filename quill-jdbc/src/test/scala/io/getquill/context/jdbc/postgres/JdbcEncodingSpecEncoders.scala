package io.getquill.context.jdbc.postgres

import java.time.LocalDateTime
import io.getquill.context.sql.EncodingSpec
import io.getquill.*
import io.getquill.context.jdbc.JdbcSpecEncoders

import java.time.ZoneId
import java.time.temporal.ChronoUnit

class JdbcEncodingSpecEncoders extends JdbcSpecEncoders with EncodingSpec {

  val context: testContext.type = testContext
  import testContext._

  "encodes and decodes types" in {
    // Why is this a dynamic query??? Does this issue happen for all deletes?
    testContext.run(delete)
    testContext.run(liftQuery(insertValues).foreach(e => insert(e)))
    verify(testContext.run(query[EncodingTestEntity]))
  }

  "encodes sets" in {
    testContext.run(query[EncodingTestEntity].delete)
    testContext.run(liftQuery(insertValues).foreach(e => query[EncodingTestEntity].insertValue(e)))
    val q = quote {
      (set: Query[Int]) =>
        query[EncodingTestEntity].filter(t => set.contains(t.v6))
    }
    verify(testContext.run(q(liftQuery(insertValues.map(_.v6)))))
  }

  "returning custom type" in {
    val uuid = testContext.run(insertBarCode(lift(barCodeEntry))).get
    val (barCode :: Nil) = testContext.run(findBarCodeByUuid(uuid))

    verifyBarcode(barCode)
  }

  // IO Monad not working yet so need to do regular queries
  "LocalDateTime" in {
    case class EncodingTestOpt(v11: Option[LocalDateTime])
    given decoder: GenericDecoder[EncodingTestOpt] = deriveDecoder

    val now = LocalDateTime.now().truncatedTo(ChronoUnit.MICROS)
    val e1 = EncodingTestOpt(Some(now))
    val e2 = EncodingTestOpt(None)
    val res: (List[EncodingTestOpt], List[EncodingTestOpt]) = {
      val steps = {
        testContext.run(query[EncodingTestOpt].delete)
        testContext.run(query[EncodingTestOpt].insertValue(lift(e1)))
        val withoutNull = testContext.run(query[EncodingTestOpt])
        testContext.run(query[EncodingTestOpt].delete)
        testContext.run(query[EncodingTestOpt].insertValue(lift(e2)))
        val withNull = testContext.run(query[EncodingTestOpt])
        (withoutNull, withNull)
      }
      steps
    }
    res._1 must contain theSameElementsAs List(EncodingTestOpt(Some(now)))
    res._2 must contain theSameElementsAs List(EncodingTestOpt(None))
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
