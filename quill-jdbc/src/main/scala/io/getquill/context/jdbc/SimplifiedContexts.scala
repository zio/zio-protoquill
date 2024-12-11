package io.getquill.context.jdbc

import java.sql.{ Connection, Types }
import io.getquill._
import io.getquill.context.ExecutionInfo
import io.getquill.util.ContextLogger

trait SqliteExecuteOverride[+D <: SqliteDialect, +N <: NamingStrategy] extends JdbcContextVerbExecute[D, N] {

  private val logger = ContextLogger(classOf[SqliteExecuteOverride[_, _]])

  private def runInTransaction[T](conn: Connection)(op: => T): T = {
    val wasAutoCommit = conn.getAutoCommit
    conn.setAutoCommit(false)
    val result = op
    try {
      conn.commit()
      result
    } catch {
      case scala.util.control.NonFatal(e) =>
        conn.rollback()
        throw e
    } finally
      conn.setAutoCommit(wasAutoCommit)
  }

  override def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(info: ExecutionInfo, dc: Runner): Result[List[T]] =
    withConnectionWrapped { conn =>
      logger.underlying.warn(
        "Sqlite does not support Batch-Actions with returning-keys. Quill will attempt to emulate this function with single-row inserts inside a transaction but using this API is not recommended."
      )
      groups.flatMap {
        case BatchGroupReturning(sql, returningBehavior, prepare, _) =>
          val ps = conn.prepareStatement(sql, java.sql.Statement.RETURN_GENERATED_KEYS)
          logger.underlying.debug("Batch: {}", sql)
          runInTransaction(conn) {
            prepare.flatMap { f =>
              val (params, _) = f(ps, conn)
              logger.logBatchItem(sql, params)
              ps.executeUpdate()
              extractResult(ps.getGeneratedKeys(), conn, extractor)
            }
          }
      }
    }
}

trait SqlServerExecuteOverride[+N <: NamingStrategy] extends JdbcContextVerbExecute[SQLServerDialect, N] {

  private val logger = ContextLogger(classOf[SqlServerExecuteOverride[_]])

  override def executeActionReturningMany[O](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[O], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner): Result[List[O]] =
    withConnectionWrapped { conn =>
      val (params, ps) = prepare(prepareWithReturning(sql, conn, returningBehavior), conn)
      logger.logQuery(sql, params)
      extractResult(ps.executeQuery, conn, extractor)
    }

  override def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(info: ExecutionInfo, dc: Runner): Result[List[T]] =
    withConnectionWrapped { conn =>
      groups.flatMap {
        case BatchGroupReturning(sql, returningBehavior, prepare, _) =>
          val ps = conn.prepareStatement(sql, java.sql.Statement.RETURN_GENERATED_KEYS)
          logger.underlying.debug("Batch: {}", sql)
          val outputs =
            prepare.flatMap { f =>
              val (params, _) = f(ps, conn)
              logger.logBatchItem(sql, params)
              ps.addBatch()
              // The SQL Server drive has no ability to either go getGeneratedKeys or executeQuery
              // at the end of a sequence of addBatch calls to get all inserted keys/executed queries
              // (whether a `OUTPUT` clause is used in the Query or not). That means that in order
              // be able to get any results, we need to use extractResult(ps.executeQuery, ...)
              // on every single inserted batch! See the following mssql-jdbc issues for more detail:
              // https://github.com/microsoft/mssql-jdbc/issues/358
              // https://github.com/Microsoft/mssql-jdbc/issues/245
              // Also note that Slick specifically mentions that returning-keys is generally
              // not supported when jdbc-batching is used:
              // https://github.com/slick/slick/blob/06ccee3cdc0722adeb8bb0658afb4a0d3524b119/slick/src/main/scala/slick/jdbc/JdbcActionComponent.scala#L654
              // Therefore slick falls back to single-row-insert batching when insertion with getGeneratedKeys is used
              //
              // However, in ProtoQuill we can do a little better. In this case we take advantage of multi-row inserts
              // (via multiple VALUES clauses) each of which is a an element of the `prepares` list. That way, we only
              // need to execute `extractResult(ps.executeQuery(),...)` once per every insert-query (where each query
              // could potentially have 1000+ insert-rows via 1000 VALUES-clauses). This radically decreases
              // the number of calls that need to be made to get back IDs (and other data) of the inserted rows.
              extractResult(ps.executeQuery(), conn, extractor)
            }
          outputs
      }
    }
}
