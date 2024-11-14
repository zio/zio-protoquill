package io.getquill

import io.getquill.context.mirror.MirrorSession
import io.getquill.context.mirror.Row
import io.getquill.context.sql.SqlContext
import io.getquill.MirrorContext.*

trait MirrorTestEntities extends TestEntities {
  type SpecSession = MirrorSession
  type SpecPrepareRow = Row
  type SpecResultRow = Row

  override given testEntityEncoder: MirrorContext.GenericDecoder[TestEntity] = MirrorContext.deriveDecoder
  override given embEncoder: MirrorContext.GenericDecoder[Emb] = MirrorContext.deriveDecoder
  override given testEntityEmbEncoder: MirrorContext.GenericDecoder[TestEntityEmb] = MirrorContext.deriveDecoder
  override given testEntity2Encoder: MirrorContext.GenericDecoder[TestEntity2] = MirrorContext.deriveDecoder
  override given testEntity3Encoder: MirrorContext.GenericDecoder[TestEntity3] = MirrorContext.deriveDecoder
  override given testEntity4Encoder: MirrorContext.GenericDecoder[TestEntity4] = MirrorContext.deriveDecoder
  override given testEntity5Encoder: MirrorContext.GenericDecoder[TestEntity5] = MirrorContext.deriveDecoder
  override given embSingleEncoder: MirrorContext.GenericDecoder[EmbSingle] = MirrorContext.deriveDecoder
  override given testEntity4EmbEncoder: MirrorContext.GenericDecoder[TestEntity4Emb] = MirrorContext.deriveDecoder
  override given testEntityRegularEncoder: MirrorContext.GenericDecoder[TestEntityRegular] = MirrorContext.deriveDecoder
}

