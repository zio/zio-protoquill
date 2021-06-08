package io.getquill.context.qzio

import io.getquill.NamingStrategy
import io.getquill.context.PrepareContext
import io.getquill.context.ZioJdbc._
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.RowContext
import io.getquill.context.ExecutionInfo
import io.getquill.util.ContextLogger
import zio.blocking.Blocking
import zio.{ Has, ZIO }

import java.sql.{ Connection, PreparedStatement, ResultSet, SQLException }

trait ZioPrepareContext[Dialect <: SqlIdiom, Naming <: NamingStrategy] extends ZioContext[Dialect, Naming]
  with PrepareContext[Dialect, Naming]
  with RowContext {

  private[getquill] val logger = ContextLogger(classOf[ZioPrepareContext[_, _]])

  override type PrepareRow = PreparedStatement
  override type ResultRow = ResultSet
  override type PrepareQueryResult = QIO[PrepareRow]
  override type PrepareActionResult = QIO[PrepareRow]
  override type PrepareBatchActionResult = QIO[List[PrepareRow]]

  def prepareQuery(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: DatasourceContext): PrepareQueryResult =
    prepareSingle(sql, prepare)(executionInfo, dc)

  def prepareAction(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: DatasourceContext): PrepareActionResult =
    prepareSingle(sql, prepare)(executionInfo, dc)

  /** Execute SQL on connection and return prepared statement. Closes the statement in a bracket. */
  def prepareSingle(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: DatasourceContext): QIO[PreparedStatement] = {
    ZIO.environment[QConnection]
      .mapEffect(bconn => bconn.get[Connection].prepareStatement(sql))
      .mapEffect { stmt =>
        val (params, ps) = prepare(stmt)
        logger.logQuery(sql, params)
        ps
      }.refineToOrDie[SQLException]
  }

  def prepareBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, dc: DatasourceContext): PrepareBatchActionResult =
    ZIO.collectAll[Has[Connection] with Blocking, Throwable, PrepareRow, List] {
      val batches = groups.flatMap {
        case BatchGroup(sql, prepares) =>
          prepares.map(sql -> _)
      }
      batches.map {
        case (sql, prepare) =>
          prepareSingle(sql, prepare)(executionInfo, dc)
      }
    }.refineToOrDie[SQLException]
}
