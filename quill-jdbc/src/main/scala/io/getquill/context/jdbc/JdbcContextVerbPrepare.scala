package io.getquill.context.jdbc

import io.getquill.*
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.{Context, ContextVerbPrepare, ContextVerbPrepareLambda, ExecutionInfo}

import java.sql.*
import io.getquill.util.ContextLogger


trait JdbcContextVerbPrepare[+Dialect <: SqlIdiom, +Naming <: NamingStrategy]
  extends ContextVerbPrepare[Dialect, Naming]
  with  Context[Dialect, Naming]
  with JdbcContextTypes  {

  override type PrepareQueryResult = Connection => Result[PreparedStatement]
  override type PrepareActionResult = Connection => Result[PreparedStatement]
  override type PrepareBatchActionResult = Connection => Result[List[PreparedStatement]]

  def constructPrepareQuery(f: Connection => Result[PreparedStatement]): PrepareQueryResult
  def constructPrepareAction(f: Connection => Result[PreparedStatement]): PrepareActionResult
  def constructPrepareBatchAction(f: Connection => Result[List[PreparedStatement]]): PrepareBatchActionResult

  private[getquill] val logger = ContextLogger(classOf[JdbcContext[_, _]])

  def wrap[T](t: => T): Result[T]
  def push[A, B](result: Result[A])(f: A => B): Result[B]
  def seq[A](list: List[Result[A]]): Result[List[A]]

  def prepareQuery(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): PrepareQueryResult =
    constructPrepareQuery(prepareSingle(sql, prepare)(executionInfo, dc))

  def prepareAction(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): PrepareActionResult =
    constructPrepareAction(prepareSingle(sql, prepare)(executionInfo, dc))

  def prepareSingle(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): Connection => Result[PreparedStatement] =
    (conn: Connection) => wrap {
      val (params, ps) = prepare(conn.prepareStatement(sql), conn)
      logger.logQuery(sql, params)
      ps
    }

  def prepareBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, dc: Runner): PrepareBatchActionResult =
    constructPrepareBatchAction {
      (session: Connection) =>
        seq {
          val batches = groups.flatMap {
            case BatchGroup(sql, prepares, _) =>
              prepares.map(sql -> _)
          }
          batches.map {
            case (sql, prepare) =>
              val prepareSql = prepareSingle(sql, prepare)(executionInfo, dc)
              prepareSql(session)
          }
        }
    }

}
