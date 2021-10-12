package io.getquill.context.jdbc

import java.io.Closeable
import java.sql.{ Connection, PreparedStatement }

import javax.sql.DataSource
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.{ NamingStrategy, ReturnAction }
import io.getquill.context.{ ContextEffect }

import scala.util.{ DynamicVariable, Try }
import scala.util.control.NonFatal
import io.getquill.context.ExecutionInfo

abstract class JdbcContext[Dialect <: SqlIdiom, Naming <: NamingStrategy]
  extends JdbcContextBase[Dialect, Naming]
  //with TranslateContext // Not defined for Dotty Quill Yet
{

  val dataSource: DataSource with Closeable

  override type Result[T] = T
  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult = Long
  override type RunActionReturningResult[T] = T
  override type RunBatchActionResult = List[Long]
  override type RunBatchActionReturningResult[T] = List[T]

  override protected val effect: ContextEffect[Result] = new ContextEffect[Result] {
    override def wrap[T](t: => T): T = t
    override def push[A, B](result: A)(f: A => B): B = f(result)
    override def seq[A](list: List[A]): List[A] = list
  }

  // In Scala2 needed explicit typing here due to scala/bug#8356. Need to check if this is still needed.
  override def executeAction[T](sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): Long =
    super.executeAction(sql, prepare)(executionInfo, dc)
  override def executeQuery[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(executionInfo: ExecutionInfo, dc: Runner): List[T] =
    super.executeQuery(sql, prepare, extractor)(executionInfo, dc)
  // override def executeQuerySingle[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor): T =
  //   super.executeQuerySingle(sql, prepare, extractor)
  override def executeActionReturning[O](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[O], returningBehavior: ReturnAction)(executionInfo: ExecutionInfo, dc: Runner): O =
    super.executeActionReturning(sql, prepare, extractor, returningBehavior)(executionInfo, dc)
  override def executeBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, dc: Runner): List[Long] =
    super.executeBatchAction(groups)(executionInfo, dc)
  override def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(executionInfo: ExecutionInfo, dc: Runner): List[T] =
   super.executeBatchActionReturning(groups, extractor)(executionInfo, dc)

  protected val currentConnection = new DynamicVariable[Option[Connection]](None)

  protected def withConnection[T](f: Connection => Result[T]) =
    currentConnection.value.map(f).getOrElse {
      val conn = dataSource.getConnection
      try f(conn)
      finally conn.close()
    }

  override def close() = dataSource.close()

  def probe(sql: String) =
    Try {
      withConnection(_.createStatement.execute(sql))
    }

  def transaction[T](f: => T) =
    currentConnection.value match {
      case Some(_) => f // already in transaction
      case None =>
        withConnection { conn =>
          currentConnection.withValue(Some(conn)) {
            val wasAutoCommit = conn.getAutoCommit
            conn.setAutoCommit(false)
            try {
              val res = f
              conn.commit()
              res
            } catch {
              case NonFatal(e) =>
                conn.rollback()
                throw e
            } finally
              conn.setAutoCommit(wasAutoCommit)
          }
        }
    }

  // Quill IO Monad Not defined in Dotty Quill
  // override def performIO[T](io: IO[T, _], transactional: Boolean = false): Result[T] =
  //   transactional match {
  //     case false => super.performIO(io)
  //     case true  => transaction(super.performIO(io))
  //   }

  // Preparation not defined in Dotty Quill Yet
  // override private[getquill] def prepareParams(statement: String, prepare: Prepare): Seq[String] = {
  //   withConnectionWrapped { conn =>
  //     prepare(conn.prepareStatement(statement))._1.reverse.map(prepareParam)
  //   }
  // }
}
