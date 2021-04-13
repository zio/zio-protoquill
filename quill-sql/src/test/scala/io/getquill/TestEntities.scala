package io.getquill

import io.getquill.context.Context
import io.getquill.quat.Quat
import io.getquill.Dsl._

trait TestEntities {
  // Has a context self type in Scala2-Quill however, for Protoquill
  // Dialect, Naming type requirements are stricter so trying to avoid the use of Context[_, _] self type

  case class TestEntity(s: String, i: Int, l: Long, o: Option[Int], b: Boolean)
  case class Emb(s: String, i: Int) extends Embedded
  case class TestEntityEmb(emb: Emb, l: Long, o: Option[Int])
  case class TestEntity2(s: String, i: Int, l: Long, o: Option[Int])
  case class TestEntity3(s: String, i: Int, l: Long, o: Option[Int])
  case class TestEntity4(i: Long)
  case class TestEntity5(i: Long, s: String)
  case class EmbSingle(i: Long) extends Embedded
  case class TestEntity4Emb(emb: EmbSingle)
  case class TestEntityRegular(s: String, i: Long)

  private val QV = Quat.Value
  private val QBV = Quat.BooleanValue

  val TestEntityQuat = Quat.Product("s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
  val TestEntityEmbQuat = Quat.Product("emb" -> Quat.Product("s" -> QV, "i" -> QV), "l" -> QV, "o" -> QV)
  val TestEntity2Quat = Quat.Product("s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV)
  val TestEntity3Quat = Quat.Product("s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV)
  val TestEntity4Quat = Quat.Product("i" -> QV)
  val TestEntity5Quat = Quat.Product("i" -> QV, "s" -> QV)
  val TestEntity4EmbQuat = Quat.Product("emb" -> Quat.Product("i" -> QV))

  inline def qr1 = quote {
    query[TestEntity]
  }
  inline def qr1Emb = quote {
    querySchema[TestEntityEmb]("TestEntity")
  }
  inline def qr2 = quote {
    query[TestEntity2]
  }
  inline def qr3 = quote {
    query[TestEntity3]
  }
  inline def qr4 = quote {
    query[TestEntity4]
  }
  inline def qr5 = quote {
    query[TestEntity5]
  }
  inline def qr4Emb = quote {
    querySchema[TestEntity4Emb]("TestEntity4")
  }
  inline def qrRegular = quote {
    for {
      a <- query[TestEntity]
    } yield TestEntityRegular(a.s, a.l)
  }
}
