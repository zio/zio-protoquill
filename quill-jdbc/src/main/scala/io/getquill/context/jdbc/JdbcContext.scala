package io.getquill.context.jdbc

import java.io.Closeable
import java.sql.{Connection, PreparedStatement}
import javax.sql.DataSource
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill._
import io.getquill.context.{ExecutionInfo, ProtoContextSecundus, ContextVerbTranslate}

import scala.util.{DynamicVariable, Try}
import scala.util.control.NonFatal
import io.getquill.Quoted
import scala.annotation.targetName
import io.getquill.context.ContextVerbTranslate
import io.getquill.util.ContextLogger

abstract class JdbcContext[+Dialect <: SqlIdiom, +Naming <: NamingStrategy]
    extends JdbcContextBase[Dialect, Naming]
    with ProtoContextSecundus[Dialect, Naming]
    with ContextVerbTranslate[Dialect, Naming] {

  private val logger = ContextLogger(classOf[JdbcContext[_, _]])

  // Need to override these with same values as JdbcRunContext because SyncIOMonad imports them. The imported values need to be overridden
  override type Result[T]                        = T
  override type RunQueryResult[T]                = List[T]
  override type RunQuerySingleResult[T]          = T
  override type RunActionResult                  = Long
  override type RunActionReturningResult[T]      = T
  override type RunBatchActionResult             = List[Long]
  override type RunBatchActionReturningResult[T] = List[T]
  override type Runner                           = Unit
  override type TranslateRunner                  = Unit
  override protected def context: Runner = ()
  def translateContext: TranslateRunner  = ()

  val dataSource: DataSource

  @targetName("runQueryDefault")
  inline def run[T](inline quoted: Quoted[Query[T]]): List[T] = InternalApi.runQueryDefault(quoted)
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): List[T] =
    InternalApi.runQuery(quoted, wrap)
  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): T = InternalApi.runQuerySingle(quoted)
  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): Long = InternalApi.runAction(quoted)
  @targetName("runActionReturning")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, T]]): T = InternalApi.runActionReturning[E, T](quoted)
  @targetName("runActionReturningMany")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, List[T]]]): List[T] =
    InternalApi.runActionReturningMany[E, T](quoted)
  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](
    inline quoted: Quoted[BatchAction[A]],
    rowsPerBatch: Int
  ): List[Long] = InternalApi.runBatchAction(quoted, rowsPerBatch)
  @targetName("runBatchActionDefault")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): List[Long] =
    InternalApi.runBatchAction(quoted, 1)
  @targetName("runBatchActionReturning")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]], rowsPerBatch: Int): List[T] =
    InternalApi.runBatchActionReturning(quoted, rowsPerBatch)
  @targetName("runBatchActionReturningDefault")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): List[T] =
    InternalApi.runBatchActionReturning(quoted, 1)

  override def wrap[T](t: => T): T                 = t
  override def push[A, B](result: A)(f: A => B): B = f(result)
  override def seq[A](list: List[A]): List[A]      = list

  protected val currentConnection = new DynamicVariable[Option[Connection]](None)

  protected def withConnection[T](f: Connection => Result[T]) =
    currentConnection.value.map(f).getOrElse {
      val conn = dataSource.getConnection
      try f(conn)
      finally conn.close()
    }

  override def close() =
    dataSource match {
      case closeable: java.io.Closeable =>
        closeable.close()
      case _ =>
        logger.underlying.warn(
          s"Could not close the DataSource `$dataSource`. It is not an instance of java.io.Closeable."
        )
    }

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
            } finally conn.setAutoCommit(wasAutoCommit)
          }
        }
    }

  override private[getquill] def prepareParams(statement: String, prepare: Prepare): Seq[String] =
    withConnectionWrapped { conn =>
      prepare(conn.prepareStatement(statement), conn)._1.reverse.map(prepareParam)
    }
}
