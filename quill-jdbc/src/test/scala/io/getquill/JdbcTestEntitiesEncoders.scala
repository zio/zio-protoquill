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

  override given testEntityEncoder: JdbcContext.GenericDecoder[TestEntity] = JdbcContext.deriveDecoder
  override given embEncoder: JdbcContext.GenericDecoder[Emb] = JdbcContext.deriveDecoder
  override given testEntityEmbEncoder: JdbcContext.GenericDecoder[TestEntityEmb] = JdbcContext.deriveDecoder
  override given testEntity2Encoder: JdbcContext.GenericDecoder[TestEntity2] = JdbcContext.deriveDecoder
  override given testEntity3Encoder: JdbcContext.GenericDecoder[TestEntity3] = JdbcContext.deriveDecoder
  override given testEntity4Encoder: JdbcContext.GenericDecoder[TestEntity4] = JdbcContext.deriveDecoder
  override given testEntity5Encoder: JdbcContext.GenericDecoder[TestEntity5] = JdbcContext.deriveDecoder
  override given embSingleEncoder: JdbcContext.GenericDecoder[EmbSingle] = JdbcContext.deriveDecoder
  override given testEntity4EmbEncoder: JdbcContext.GenericDecoder[TestEntity4Emb] = JdbcContext.deriveDecoder
  override given testEntityRegularEncoder: JdbcContext.GenericDecoder[TestEntityRegular] = JdbcContext.deriveDecoder
}
