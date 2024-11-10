package io.getquill

import com.datastax.oss.driver.api.core.CqlSession
import com.datastax.oss.driver.api.core.cql.Row
import com.typesafe.config.Config
import io.getquill.context.ExecutionInfo
import io.getquill.context.cassandra.{CassandraCodecsBase, CqlIdiom}
import io.getquill.util.{ContextLogger, LoadConfig}

import scala.jdk.CollectionConverters.*
import scala.annotation.targetName

object CassandraSyncContext extends CassandraCodecsBase[CqlSession]

class CassandraSyncContext[+N <: NamingStrategy](
  naming:                     N,
  session:                    CqlSession,
  preparedStatementCacheSize: Long
) extends CassandraCqlSessionContext[N](naming, session, preparedStatementCacheSize) {
  val idiom = CqlIdiom

  export CassandraSyncContext.{
    PrepareRow => _,
    ResultRow => _,
    Session => _,
    _
  }

  def this(naming: N, config: CassandraContextConfig) = this(naming, config.session, config.preparedStatementCacheSize)
  def this(naming: N, config: Config) = this(naming, CassandraContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  private val logger = ContextLogger(classOf[CassandraSyncContext[_]])

  override type Result[T] = T
  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult = Unit
  override type RunBatchActionResult = Unit
  override type Runner = Unit

  @targetName("runQueryDefault")
  inline def run[T](inline quoted: Quoted[Query[T]]): List[T] = InternalApi.runQueryDefault(quoted)
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): List[T] = InternalApi.runQuery(quoted, wrap)
  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): T = InternalApi.runQuerySingle(quoted)
  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): Unit = InternalApi.runAction(quoted)
  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): Unit = InternalApi.runBatchAction(quoted, 1)

  override protected def context: Runner = ()

  // override def performIO[T](io: IO[T, _], transactional: Boolean = false): Result[T] = {
  //   if (transactional) logger.underlying.warn("Cassandra doesn't support transactions, ignoring `io.transactional`")
  //   super.performIO(io)
  // }

  def executeQuery[T](cql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner): List[T] = {
    val (params, bs) = prepare(this.prepare(cql), this)
    logger.logQuery(cql, params)
    session.execute(bs)
      .all.asScala.toList.map(row => extractor(row, this))
  }

  def executeQuerySingle[T](cql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner): T =
    handleSingleResult(cql, executeQuery(cql, prepare, extractor)(info, dc))

  def executeAction(cql: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner): Unit = {
    val (params, bs) = prepare(this.prepare(cql), this)
    logger.logQuery(cql, params)
    session.execute(bs)
    ()
  }

  def executeBatchAction(groups: List[BatchGroup])(info: ExecutionInfo, dc: Runner): Unit =
    groups.foreach {
      case BatchGroup(cql, prepare) =>
        prepare.foreach(executeAction(cql, _)(info, dc))
    }
}
