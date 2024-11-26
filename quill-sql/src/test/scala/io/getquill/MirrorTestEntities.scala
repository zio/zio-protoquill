package io.getquill

import io.getquill.context.mirror.MirrorSession
import io.getquill.context.mirror.Row
import io.getquill.context.sql.SqlContext
import io.getquill.MirrorContext.*

trait MirrorTestEntities extends TestEntities {
  type SpecSession = MirrorSession
  type SpecPrepareRow = Row
  type SpecResultRow = Row

  override given testEntityEncoder: MirrorContext.CompositeDecoder[TestEntity] = MirrorContext.deriveComposite
  override given embEncoder: MirrorContext.CompositeDecoder[Emb] = MirrorContext.deriveComposite
  override given testEntityEmbEncoder: MirrorContext.CompositeDecoder[TestEntityEmb] = MirrorContext.deriveComposite
  override given testEntity2Encoder: MirrorContext.CompositeDecoder[TestEntity2] = MirrorContext.deriveComposite
  override given testEntity3Encoder: MirrorContext.CompositeDecoder[TestEntity3] = MirrorContext.deriveComposite
  override given testEntity4Encoder: MirrorContext.CompositeDecoder[TestEntity4] = MirrorContext.deriveComposite
  override given testEntity5Encoder: MirrorContext.CompositeDecoder[TestEntity5] = MirrorContext.deriveComposite
  override given embSingleEncoder: MirrorContext.CompositeDecoder[EmbSingle] = MirrorContext.deriveComposite
  override given testEntity4EmbEncoder: MirrorContext.CompositeDecoder[TestEntity4Emb] = MirrorContext.deriveComposite
  override given testEntityRegularEncoder: MirrorContext.CompositeDecoder[TestEntityRegular] = MirrorContext.deriveComposite
}

