package io.getquill

import io.getquill.context.Context
import io.getquill.generic.{DatabaseVerbs, DecodingType, GenericDecoder}
import io.getquill.quat.Quat

trait TestEntities extends DatabaseVerbs { self: Spec =>

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
  given testEntityEncoder: GenericDecoder[ResultRow, Session, TestEntity, DecodingType.Composite] = deriveComposite
  given embEncoder: GenericDecoder[ResultRow, Session, Emb, DecodingType.Composite] = deriveComposite
  given testEntityEmbEncoder: GenericDecoder[ResultRow, Session, TestEntityEmb, DecodingType.Composite] = deriveComposite
  given testEntity2Encoder: GenericDecoder[ResultRow, Session, TestEntity2, DecodingType.Composite] = deriveComposite
  given testEntity3Encoder: GenericDecoder[ResultRow, Session, TestEntity3, DecodingType.Composite] = deriveComposite
  given testEntity4Encoder: GenericDecoder[ResultRow, Session, TestEntity4, DecodingType.Composite] = deriveComposite
  given testEntity5Encoder: GenericDecoder[ResultRow, Session, TestEntity5, DecodingType.Composite] = deriveComposite
  given embSingleEncoder: GenericDecoder[ResultRow, Session, EmbSingle, DecodingType.Composite] = deriveComposite
  given testEntity4EmbEncoder: GenericDecoder[ResultRow, Session, TestEntity4Emb, DecodingType.Composite] = deriveComposite
  given testEntityRegularEncoder: GenericDecoder[ResultRow, Session, TestEntityRegular, DecodingType.Composite] = deriveComposite

  private val QV = Quat.Value
  private val QBV = Quat.BooleanValue

  val TestEntityQuat = Quat.Product("TestEntity", "s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
  val TestEntityEmbQuat = Quat.Product("TestEntityEmb", "emb" -> Quat.Product("Emb", "s" -> QV, "i" -> QV), "l" -> QV, "o" -> QV)
  val TestEntity2Quat = Quat.Product("TestEntity2", "s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV)
  val TestEntity3Quat = Quat.Product("TestEntity3", "s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV)
  val TestEntity4Quat = Quat.Product("TestEntity4", "i" -> QV)
  val TestEntity5Quat = Quat.Product("TestEntity5", "i" -> QV, "s" -> QV)
  val TestEntity4EmbQuat = Quat.Product("TestEntity4Emb", "emb" -> Quat.Product("EmbSingle", "i" -> QV))

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
