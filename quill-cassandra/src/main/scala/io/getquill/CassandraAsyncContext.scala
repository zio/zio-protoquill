package io.getquill

import com.datastax.driver.core.Cluster
import com.typesafe.config.Config
import io.getquill.context.ExecutionInfo
import io.getquill.context.cassandra.util.FutureConversions._
//import io.getquill.monad.ScalaFutureIOMonad
import io.getquill.util.{ ContextLogger, LoadConfig }
import io.getquill.context.RunnerSummoningBehavior

import scala.jdk.CollectionConverters._
import scala.concurrent.{ ExecutionContext, Future }

class CassandraAsyncContext[N <: NamingStrategy]( //hello
  naming:                     N,
  cluster:                    Cluster,
  keyspace:                   String,
  preparedStatementCacheSize: Long
)
  extends CassandraClusterSessionContext[N](naming, cluster, keyspace, preparedStatementCacheSize)
  /*with ScalaFutureIOMonad*/ {

  // The ProtoQuill way of doing `implicit ec: ExceutionContext`.
  // This will cause the Context.scala `run` functions etc... summon an implicit
  // Runner from the context which we set to ExecutionContext here.
  // That is because Dotty macros cannot do arbitrary things like adding implicit
  // parameters to functions (which the Scala2-Quill implementation relied on
  // to summon an ExecutionContext).
  override type RunnerSummoning = RunnerSummoningBehavior.Implicit
  override type Runner = ExecutionContext

  def this(naming: N, config: CassandraContextConfig) = this(naming, config.cluster, config.keyspace, config.preparedStatementCacheSize)

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

  // override def performIO[T](io: IO[T, _], transactional: Boolean = false)(implicit ec: ExecutionContext): Result[T] = {
  //   if (transactional) logger.underlying.warn("Cassandra doesn't support transactions, ignoring `io.transactional`")
  //   super.performIO(io)
  // }

  def executeQuery[T](cql: String, prepare: Prepare, extractor: Extractor[T])(executionInfo: ExecutionInfo, dc: ExecutionContext): Result[RunQueryResult[T]] = {
    implicit val ec = dc
    val statement = prepareAsyncAndGetStatement(cql, prepare, this, logger)
    statement.flatMap(st => session.executeAsync(st).asScala)
      .map(_.all.asScala.toList.map(row => extractor(row, this)))
  }

  def executeQuerySingle[T](cql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(executionInfo: ExecutionInfo, dc: Runner): Result[RunQuerySingleResult[T]] = {
    implicit val ec = dc
    executeQuery(cql, prepare, extractor)(executionInfo, dc).map(handleSingleResult)
  }

  def executeAction[T](cql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): Result[RunActionResult] = {
    implicit val ec = dc
    val statement = prepareAsyncAndGetStatement(cql, prepare, this, logger)
    statement.flatMap(st => session.executeAsync(st).asScala).map(_ => ())
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
