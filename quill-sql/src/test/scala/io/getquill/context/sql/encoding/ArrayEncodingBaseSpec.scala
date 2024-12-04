package io.getquill.context.sql.encoding

import io.getquill.context.sql.SqlContext

import java.time.LocalDate
import java.util.Date
import java.util.UUID
import io.getquill.{MappedEncoding, ProductDecoders, Spec, toDecoder, toEncoder, toSeqDecoder, toSeqEncoder}
import org.scalatest.{Assertion, BeforeAndAfterEach}
import io.getquill.context.Context
import io.getquill.generic.{DecodingType, GenericDecoder, GenericEncoder, GenericNullChecker, ArrayEncoding}

// TODO create a standard SpecEncoders trait that can be mixed in to all tests so we don't need to declared base encoders
trait ArrayEncodingBaseSpec extends Spec with BeforeAndAfterEach with ArrayEncoding { self =>
  val context: SqlContext[_, _] {
    type Session = self.Session
    type PrepareRow = self.PrepareRow
    type ResultRow = self.ResultRow
  }

  import context._

  // Support all sql base types and `Seq` implementers
  case class ArraysTestEntity(
      texts: List[String],
      decimals: Seq[BigDecimal],
      bools: Vector[Boolean],
      bytes: List[Byte],
      shorts: IndexedSeq[Short],
      ints: Seq[Int],
      longs: Seq[Long],
      floats: Seq[Float],
      doubles: Seq[Double],
      timestamps: Seq[Date],
      dates: Seq[LocalDate],
      uuids: Seq[UUID]
  )

  val e = ArraysTestEntity(
    List("test"),
    Seq(BigDecimal(2.33)),
    Vector(true, true),
    List(1),
    IndexedSeq(3),
    Seq(2),
    Seq(1, 2, 3),
    Seq(1f, 2f),
    Seq(4d, 3d),
    Seq(new Date(System.currentTimeMillis())),
    Seq(LocalDate.now()),
    Seq(UUID.randomUUID())
  )

  // casting types can be dangerous so we need to ensure that everything is ok
  def baseEntityDeepCheck(e1: ArraysTestEntity, e2: ArraysTestEntity): Assertion = {
    e1.texts.head mustBe e2.texts.head
    e1.decimals.head mustBe e2.decimals.head
    e1.bools.head mustBe e2.bools.head
    e1.bytes.head mustBe e2.bytes.head
    e1.shorts.head mustBe e2.shorts.head
    e1.ints.head mustBe e2.ints.head
    e1.longs.head mustBe e2.longs.head
    e1.floats.head mustBe e2.floats.head
    e1.doubles.head mustBe e2.doubles.head
    e1.timestamps.head mustBe e2.timestamps.head
    e1.dates.head mustBe e2.dates.head
    e1.uuids.head mustBe e2.uuids.head
  }

  // Support Seq encoding basing on MappedEncoding
  case class StrWrap(str: String)
  // impossible to define toEncoder/toDecoder for MappedEncoding on the base-level unless Encoder is generic
  // (even then we still need to have default encoders for String)
  // This is why Encoder on base-level needs to be defined as GenericEncoder
  val wrapString = MappedEncoding { (str: String) => StrWrap(str) }
  val unwrapString = MappedEncoding { (wrap: StrWrap) => wrap.str }
  given strWrapEncode: GenericEncoder[StrWrap, PrepareRow, Session] = unwrapString.toEncoder
  given strWrapDecode: GenericDecoder[ResultRow, Session, StrWrap, DecodingType.Leaf] = wrapString.toDecoder
  given strWrapSeqEncoder(using GenericEncoder[Seq[String], PrepareRow, Session]): GenericEncoder[Seq[StrWrap], PrepareRow, Session] = unwrapString.toSeqEncoder
  given strWrapSeqDecoder(using GenericDecoder[ResultRow, Session, Seq[String], DecodingType.Leaf]): GenericDecoder[ResultRow, Session, Seq[StrWrap], DecodingType.Leaf] = wrapString.toSeqDecoder

  case class WrapEntity(texts: Seq[StrWrap])
  given wrapEntityDecoder: GenericDecoder[ResultRow, Session, WrapEntity, DecodingType.Composite] = deriveComposite
  val wrapE = WrapEntity(List("hey", "ho").map(StrWrap.apply))

  // TODO maybe doing the context-base `context.manual.deriveComposite` is what's making the tests slow
}
