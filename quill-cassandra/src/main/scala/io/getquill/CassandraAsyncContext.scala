package io.getquill

import com.datastax.oss.driver.api.core.{ CqlSession, CqlSessionBuilder }
import com.typesafe.config.Config
import io.getquill.context.ExecutionInfo
import io.getquill.context.cassandra.util.FutureConversions._
//import io.getquill.monad.ScalaFutureIOMonad
import io.getquill.util.{ ContextLogger, LoadConfig }
import io.getquill.context.RunnerSummoningBehavior

import scala.jdk.CollectionConverters._
import scala.compat.java8.FutureConverters._

import scala.concurrent.{ ExecutionContext, Future }
import scala.annotation.targetName

class CassandraAsyncContext[+N <: NamingStrategy](
  naming:                     N,
  session:                    CqlSession,
  preparedStatementCacheSize: Long
)
  extends CassandraCqlSessionContext[N](naming, session, preparedStatementCacheSize)
  /*with ScalaFutureIOMonad*/ {

  // The ProtoQuill way of doing `implicit ec: ExecutionContext`.
  // This will cause the Context.scala `run` functions etc... summon an implicit
  // Runner from the context which we set to ExecutionContext here.
  // That is because Dotty macros cannot do arbitrary things like adding implicit
  // parameters to functions (which the Scala2-Quill implementation relied on
  // to summon an ExecutionContext).
  override type RunnerBehavior = RunnerSummoningBehavior.Implicit
  override type Runner = ExecutionContext

  def this(naming: N, config: CassandraContextConfig) = this(naming, config.session, config.preparedStatementCacheSize)

  def this(naming: N, config: Config) = this(naming, CassandraContextConfig(config))

  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  private val logger = ContextLogger(classOf[CassandraAsyncContext[_]])

  override type Result[T] = Future[T]
  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult = Unit
  override type RunBatchActionResult = Unit
  // In ProtoQuill this is defined in CassandraRowContext and the Runner is ExecutionContext
  // override type Runner = Unit

  @targetName("runQueryDefault")
  inline def run[T](inline quoted: Quoted[Query[T]]): Future[List[T]] = InternalApi.runQueryDefault(quoted)
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): Future[List[T]] = InternalApi.runQuery(quoted, wrap)
  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): Future[T] = InternalApi.runQuerySingle(quoted)
  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): Future[Unit] = InternalApi.runAction(quoted)
  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): Future[Unit] = InternalApi.runBatchAction(quoted, 1)

  // override def performIO[T](io: IO[T, _], transactional: Boolean = false)(implicit ec: ExecutionContext): Result[T] = {
  //   if (transactional) logger.underlying.warn("Cassandra doesn't support transactions, ignoring `io.transactional`")
  //   super.performIO(io)
  // }

  def executeQuery[T](cql: String, prepare: Prepare, extractor: Extractor[T])(info: ExecutionInfo, dc: ExecutionContext): Result[RunQueryResult[T]] = {
    implicit val ec = dc
    val statement = prepareAsyncAndGetStatement(cql, prepare, this, logger)
    statement.map(st => session.execute(st).asScala.toList.map(row => extractor(row, this)))
  }

  def executeQuerySingle[T](cql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner): Result[RunQuerySingleResult[T]] = {
    implicit val ec = dc
    executeQuery(cql, prepare, extractor)(info, dc).map(handleSingleResult(cql, _))
  }

  def executeAction(cql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): Result[RunActionResult] = {
    implicit val ec = dc
    val statement = prepareAsyncAndGetStatement(cql, prepare, this, logger)
    statement.flatMap(st => session.executeAsync(st).toCompletableFuture.toScala).map(_ => ())
  }

  def executeBatchAction(groups: List[BatchGroup])(info: ExecutionInfo, dc: Runner): Result[RunBatchActionResult] = {
    implicit val ec = dc
    Future.sequence {
      groups.flatMap {
        case BatchGroup(cql, prepare) =>
          prepare.map(executeAction(cql, _)(info, dc))
      }
    }.map(_ => ())
  }

}
