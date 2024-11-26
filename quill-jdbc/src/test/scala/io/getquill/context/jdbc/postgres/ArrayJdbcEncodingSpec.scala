package io.getquill.context.jdbc.postgres

import java.sql.Timestamp
import java.time.LocalDate
import java.util.UUID
import io.getquill.context.sql.encoding.ArrayEncodingBaseSpec
import io.getquill.*
import io.getquill.PostgresJdbcContext.*
import io.getquill.context.jdbc.PostgresJdbcSpecEncoders

class ArrayJdbcEncodingSpec extends ArrayEncodingBaseSpec with PostgresJdbcSpecEncoders {
  val ctx = testContext
  import ctx._

  ///home/alexi/git/protoquill/quill-jdbc/src/test/scala/io/getquill/context/jdbc/postgres/ArrayJdbcEncodingSpec.scala:15:60
  //No given instance of type io.getquill.MappedEncoding[String, String] was found for parameter x of method summon in object Predef
  //val encodeStrStr = summon[MappedEncoding[String, String]]
  //def encodeStrStr[O] = summon[MappedEncoding[O, String]]
  //def foo[O] = SummonLog[String]

  inline def q = quote(query[ArraysTestEntity])
  val corrected = e.copy(timestamps = e.timestamps.map(d => new Timestamp(d.getTime)))

  // If we re-enable the StrWrap mapped-encoding/decoding then this will blow up with a conflict for Decoder[Seq[I]]
  // is it trying to derive something for Seq[I] because it thinks Seq is a product or sum type???
  // Or maybe StrWrap is being derived as something???
  given PostgresJdbcContext.CompositeDecoder[ArraysTestEntity] = PostgresJdbcContext.deriveComposite

  "Support all sql base types and `Seq` implementers" in {
    ctx.run(q.insertValue(lift(corrected)))
    val actual = ctx.run(q).head
    actual mustEqual corrected
    baseEntityDeepCheck(actual, corrected)
  }

  "Support Seq encoding basing on MappedEncoding" in {
    val wrapQ = quote(querySchema[WrapEntity]("ArraysTestEntity"))
    ctx.run(wrapQ.insertValue(lift(wrapE)))
    ctx.run(wrapQ).head.texts mustBe wrapE.texts
  }

  "Timestamps" in {
    case class Timestamps(timestamps: List[Timestamp])
    given PostgresJdbcContext.CompositeDecoder[Timestamps] = PostgresJdbcContext.deriveComposite
    val tE = Timestamps(List(new Timestamp(System.currentTimeMillis())))
    val tQ = quote(querySchema[Timestamps]("ArraysTestEntity"))
    ctx.run(tQ.insertValue(lift(tE)))
    ctx.run(tQ).head.timestamps mustBe tE.timestamps
  }

  "Arrays in where clause" in {
    ctx.run(q.insertValue(lift(corrected)))
    val actual1 = ctx.run(q.filter(_.texts == lift(List("test"))))
    val actual2 = ctx.run(q.filter(_.texts == lift(List("test2"))))
    actual1 mustEqual List(corrected)
    actual2 mustEqual List()
  }

  "empty array on found null" in {
    case class ArraysTestEntity(texts: Option[List[String]])
    given PostgresJdbcContext.CompositeDecoder[ArraysTestEntity] = PostgresJdbcContext.deriveComposite
    ctx.run(query[ArraysTestEntity].insertValue(lift(ArraysTestEntity(None))))

    case class E(texts: List[String])
    ctx.run(querySchema[E]("ArraysTestEntity")).headOption.map(_.texts) mustBe Some(Nil)
  }

  override protected def beforeEach(): Unit = {
    ctx.run(q.delete)
    ()
  }
}
