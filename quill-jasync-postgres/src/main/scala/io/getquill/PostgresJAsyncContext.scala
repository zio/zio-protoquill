package io.getquill

import com.github.jasync.sql.db.pool.ConnectionPool
import com.github.jasync.sql.db.postgresql.PostgreSQLConnection
import com.github.jasync.sql.db.{ QueryResult => DBQueryResult }
import com.typesafe.config.Config
import io.getquill.ReturnAction.{ ReturnColumns, ReturnNothing, ReturnRecord }
import io.getquill.context.jasync.{ ArrayDecoders, ArrayEncoders, JAsyncContext, JAsyncContextBase, UUIDObjectEncoding }
import io.getquill.util.LoadConfig
import io.getquill.util.Messages.fail

import scala.jdk.CollectionConverters._
import scala.annotation.targetName
import scala.concurrent.Future

trait PostgresJAsyncContextBase[N <: NamingStrategy]
  extends JAsyncContextBase[PostgresDialect, N]
  with ArrayEncoders
  with ArrayDecoders
  with UUIDObjectEncoding

class PostgresJAsyncContext[N <: NamingStrategy](naming: N, pool: ConnectionPool[PostgreSQLConnection])
  extends JAsyncContext[PostgresDialect, N, PostgreSQLConnection](PostgresDialect, naming, pool)
  with PostgresJAsyncContextBase[N] {

  def this(naming: N, config: PostgresJAsyncContextConfig) = this(naming, config.pool)
  def this(naming: N, config: Config) = this(naming, PostgresJAsyncContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  @targetName("runQueryDefault")
  inline def run[T](inline quoted: Quoted[Query[T]]): Future[Seq[T]] = InternalApi.runQueryDefault(quoted)
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): Future[Seq[T]] = InternalApi.runQuery(quoted, wrap)
  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): Future[T] = InternalApi.runQuerySingle(quoted)
  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): Future[Long] = InternalApi.runAction(quoted)
  @targetName("runActionReturning")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, T]]): Future[T] = InternalApi.runActionReturning[E, T](quoted)
  @targetName("runActionReturningMany")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, List[T]]]): Future[List[T]] = InternalApi.runActionReturningMany[E, T](quoted)
  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): Future[Seq[Long]] = InternalApi.runBatchAction(quoted)
  @targetName("runBatchActionReturning")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): Future[Seq[T]] =  InternalApi.runBatchActionReturning(quoted)

  override protected def extractActionResult[O](returningAction: ReturnAction, returningExtractor: Extractor[O])(result: DBQueryResult): List[O] =
    result.getRows.asScala.toList.map(row => returningExtractor(row, ()))

  override protected def expandAction(sql: String, returningAction: ReturnAction): String =
    returningAction match {
      // The Postgres dialect will create SQL that has a 'RETURNING' clause so we don't have to add one.
      case ReturnRecord           => s"$sql"
      // The Postgres dialect will not actually use these below variants but in case we decide to plug
      // in some other dialect into this context...
      case ReturnColumns(columns) => s"$sql RETURNING ${columns.mkString(", ")}"
      case ReturnNothing          => s"$sql"
    }
}
