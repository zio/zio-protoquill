package io.getquill

import io.getquill.context.sql.SqlContext
import io.getquill.generic.{DecodingType, GenericDecoder}
import io.getquill.context.jdbc.{JdbcContext, JdbcSpecEncoders}

import java.sql.{Connection, PreparedStatement, ResultSet}

trait JdbcTestEntitiesEncoders extends JdbcSpecEncoders with TestEntities {
  type SpecSession = Connection
  type SpecPrepareRow = PreparedStatement
  type SpecResultRow = ResultSet

  val context: SqlContext[_, _] {
    type Session = SpecSession
    type PrepareRow = SpecPrepareRow
    type ResultRow = SpecResultRow
  }

  override given testEntityEncoder: JdbcContext.CompositeDecoder[TestEntity] = JdbcContext.deriveComposite
  override given embEncoder: JdbcContext.CompositeDecoder[Emb] = JdbcContext.deriveComposite
  override given testEntityEmbEncoder: JdbcContext.CompositeDecoder[TestEntityEmb] = JdbcContext.deriveComposite
  override given testEntity2Encoder: JdbcContext.CompositeDecoder[TestEntity2] = JdbcContext.deriveComposite
  override given testEntity3Encoder: JdbcContext.CompositeDecoder[TestEntity3] = JdbcContext.deriveComposite
  override given testEntity4Encoder: JdbcContext.CompositeDecoder[TestEntity4] = JdbcContext.deriveComposite
  override given testEntity5Encoder: JdbcContext.CompositeDecoder[TestEntity5] = JdbcContext.deriveComposite
  override given embSingleEncoder: JdbcContext.CompositeDecoder[EmbSingle] = JdbcContext.deriveComposite
  override given testEntity4EmbEncoder: JdbcContext.CompositeDecoder[TestEntity4Emb] = JdbcContext.deriveComposite
  override given testEntityRegularEncoder: JdbcContext.CompositeDecoder[TestEntityRegular] = JdbcContext.deriveComposite
}
