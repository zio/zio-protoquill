package io.getquill.doobie

import cats.data.Nested
import cats.syntax.all._
import cats.free.Free
import doobie.free.connection.ConnectionOp
import doobie.{Query => DQuery, _}
import doobie.implicits._
import doobie.util.query.DefaultChunkSize
import fs2.Stream
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.ContextVerbStream
import io.getquill.context.ExecutionInfo
import java.sql.Connection
import scala.util.Success
import scala.util.Try
import doobie.enumerated.AutoGeneratedKeys
import io.getquill.ReturnAction.ReturnColumns
import io.getquill.ReturnAction.ReturnNothing
import io.getquill.ReturnAction.ReturnRecord
import io.getquill._
import io.getquill.context.jdbc.JdbcContextBase
import io.getquill.util.ContextLogger
import scala.language.implicitConversions
import io.getquill.context.jdbc.JdbcContextTypes
import io.getquill.context.ProtoContextSecundus
import scala.annotation.targetName

/** Base trait from which vendor-specific variants are derived. */
trait DoobieContextBase[+Dialect <: SqlIdiom, +Naming <: NamingStrategy]
    extends JdbcContextTypes[Dialect, Naming]
    with ProtoContextSecundus[Dialect, Naming]
    with ContextVerbStream[Dialect, Naming] {

  override type Result[A]                        = ConnectionIO[A]
  override type RunQueryResult[A]                = List[A]
  override type RunQuerySingleResult[A]          = A
  override type StreamResult[A]                  = Stream[ConnectionIO, A]
  override type RunActionResult                  = Long
  override type RunActionReturningResult[A]      = A
  override type RunBatchActionResult             = List[Long]
  override type RunBatchActionReturningResult[A] = List[A]

  override type Runner = Unit
  override protected def context: Runner = ()

  @targetName("runQueryDefault")
  inline def run[T](inline quoted: Quoted[Query[T]]): ConnectionIO[List[T]] = InternalApi.runQueryDefault(quoted)
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): ConnectionIO[List[T]] =
    InternalApi.runQuery(quoted, wrap)
  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): ConnectionIO[T] = InternalApi.runQuerySingle(quoted)
  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): ConnectionIO[Long] = InternalApi.runAction(quoted)
  @targetName("runActionReturning")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, T]]): ConnectionIO[T] =
    InternalApi.runActionReturning[E, T](quoted)
  @targetName("runActionReturningMany")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, List[T]]]): ConnectionIO[List[T]] =
    InternalApi.runActionReturningMany[E, T](quoted)
  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](
    inline quoted: Quoted[BatchAction[A]],
    rowsPerBatch: Int
  ): ConnectionIO[List[Long]] = InternalApi.runBatchAction(quoted, rowsPerBatch)
  @targetName("runBatchActionDefault")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): ConnectionIO[List[Long]] =
    InternalApi.runBatchAction(quoted, 1)
  @targetName("runBatchActionReturning")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](
    inline quoted: Quoted[BatchAction[A]],
    rowsPerBatch: Int
  ): ConnectionIO[List[T]] = InternalApi.runBatchActionReturning(quoted, rowsPerBatch)
  @targetName("runBatchActionReturningDefault")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): ConnectionIO[List[T]] =
    InternalApi.runBatchActionReturning(quoted, 1)

  // Logging behavior should be identical to JdbcContextBase.scala, which includes a couple calls
  // to log.underlying below.
  private val log: ContextLogger = new ContextLogger("DoobieContext")

  private def useConnection[A](f: Connection => PreparedStatementIO[A]): PreparedStatementIO[A] =
    FPS.getConnection.flatMap(f)

  private def prepareAndLog(
    sql: String,
    p: Prepare
  )(implicit
    connection: Connection
  ): PreparedStatementIO[Unit] = FPS.raw(p(_, connection)).flatMap { case (params, _) =>
    FPS.delay(log.logQuery(sql, params))
  }

  override def executeQuery[A](
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[A] = identityExtractor
  )(
    info: ExecutionInfo,
    dc: Runner
  ): ConnectionIO[List[A]] =
    HC.prepareStatement(sql) {
      useConnection { implicit connection =>
        prepareAndLog(sql, prepare) *>
          HPS.executeQuery {
            HRS.list(extractor)
          }
      }
    }

  override def executeQuerySingle[A](
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[A] = identityExtractor
  )(
    info: ExecutionInfo,
    dc: Runner
  ): ConnectionIO[A] =
    HC.prepareStatement(sql) {
      useConnection { implicit connection =>
        prepareAndLog(sql, prepare) *>
          HPS.executeQuery {
            HRS.getUnique(extractor)
          }
      }
    }

  def streamQuery[A](
    fetchSize: Option[Int],
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[A] = identityExtractor
  )(
    info: ExecutionInfo,
    dc: Runner
  ): Stream[ConnectionIO, A] =
    for {
      connection <- Stream.eval(FC.raw(identity))
      result <-
        HC.stream(
          sql,
          prepareAndLog(sql, prepare)(connection),
          fetchSize.getOrElse(DefaultChunkSize)
        )(extractorToRead(extractor)(connection))
    } yield result

  override def executeAction(
    sql: String,
    prepare: Prepare = identityPrepare
  )(info: ExecutionInfo, dc: Runner): ConnectionIO[Long] =
    HC.prepareStatement(sql) {
      useConnection { implicit connection =>
        prepareAndLog(sql, prepare) *>
          HPS.executeUpdate.map(_.toLong)
      }
    }

  private def prepareConnections[A](returningBehavior: ReturnAction) =
    returningBehavior match {
      case ReturnColumns(columns) => (sql: String) => HC.prepareStatementS[A](sql, columns)(_)
      case ReturnRecord =>
        (sql: String) => HC.prepareStatement[A](sql, AutoGeneratedKeys.ReturnGeneratedKeys)(_)
      case ReturnNothing => (sql: String) => HC.prepareStatement[A](sql)(_)
    }

  override def executeActionReturning[A](
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[A],
    returningBehavior: ReturnAction
  )(
    info: ExecutionInfo,
    dc: Runner
  ): ConnectionIO[A] =
    executeActionReturningMany[A](sql, prepare, extractor, returningBehavior)(info, dc).map(handleSingleResult(sql, _))

  override def executeActionReturningMany[A](
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[A],
    returningBehavior: ReturnAction
  )(
    info: ExecutionInfo,
    dc: Runner
  ): ConnectionIO[List[A]] =
    prepareConnections[List[A]](returningBehavior)(sql) {
      useConnection { implicit connection =>
        prepareAndLog(sql, prepare) *>
          FPS.executeUpdate *>
          HPS.getGeneratedKeys[List[A]](HRS.list(extractor))
      }
    }

  private def prepareBatchAndLog(
    sql: String,
    p: Prepare
  )(implicit
    connection: Connection
  ): PreparedStatementIO[Unit] =
    FPS.raw(p(_, connection)) flatMap { case (params, _) =>
      FPS.delay(log.logBatchItem(sql, params))
    }

  override def executeBatchAction(
    groups: List[BatchGroup]
  )(
    info: ExecutionInfo,
    dc: Runner
  ): ConnectionIO[List[Long]] = groups.flatTraverse { case BatchGroup(sql, preps) =>
    HC.prepareStatement(sql) {
      useConnection { implicit connection =>
        for {
          _ <- FPS.delay(log.underlying.debug("Batch: {}", sql))
          _ <- preps.traverse(prepareBatchAndLog(sql, _) *> FPS.addBatch)
          r <- Nested(HPS.executeBatch).value.map(_.map(_.toLong))
        } yield r
      }
    }
  }

  override def executeBatchActionReturning[A](
    groups: List[BatchGroupReturning],
    extractor: Extractor[A]
  )(
    info: ExecutionInfo,
    dc: Runner
  ): ConnectionIO[List[A]] = groups.flatTraverse { case BatchGroupReturning(sql, returningBehavior, preps) =>
    prepareConnections(returningBehavior)(sql) {

      useConnection { implicit connection =>
        for {
          _ <- FPS.delay(log.underlying.debug("Batch: {}", sql))
          _ <- preps.traverse(prepareBatchAndLog(sql, _) *> FPS.addBatch)
          _ <- HPS.executeBatch
          r <- HPS.getGeneratedKeys(HRS.list(extractor))
        } yield r
      }
    }
  }

  // Turn an extractor into a `Read` so we can use the existing resultset.
  private implicit def extractorToRead[A](
    ex: Extractor[A]
  )(implicit
    connection: Connection
  ): Read[A] = new Read[A](Nil, (rs, _) => ex(rs, connection))

  // Nothing to do here.
  override def close(): Unit = ()

  // Dotty Quill does not support probing yet.
  // override def probe(statement: String): Try[_] = Success(())

  // Don't need this for our particular override
  // override protected def withConnection[A](f: Connection => ConnectionIO[A]): ConnectionIO[A] = ???

  protected val effect = null

  def wrap[T](t: => T): Free[ConnectionOp, T]                                     = Free.pure(t)
  def push[A, B](result: Free[ConnectionOp, A])(f: A => B): Free[ConnectionOp, B] = result.map(f(_))
  def seq[A](list: List[Free[ConnectionOp, A]]): Free[ConnectionOp, List[A]] =
    list.sequence[[L] =>> Free[ConnectionOp, L], A]
}
