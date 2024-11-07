package io.getquill

import io.getquill.context.mirror.*
import io.getquill.context.*
import io.getquill.Quoted
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.context.sql.SqlEncoding
import io.getquill.generic.{DecodingType, EncodingDsl, GenericDecoder}

import scala.annotation.targetName

object MirrorContext extends SqlEncoding with MirrorDecoders with MirrorEncoders {
  override type Session = MirrorSession
  override type PrepareRow = Row
  override type ResultRow = Row
  override type NullChecker = MirrorNullChecker
  class MirrorNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: Row): Boolean = row.nullAt(index)
  }
  implicit val nullChecker: NullChecker = new MirrorNullChecker()

  type GenericDecoder[T] = io.getquill.generic.GenericDecoder[Row, MirrorSession, T, DecodingType.Generic]
  inline def deriveDecoder[T]: GenericDecoder[T] = ${ io.getquill.generic.GenericDecoder.summon[T, Row, MirrorSession] }
}

class MirrorContext[+Dialect <: Idiom, +Naming <: NamingStrategy](val idiom: Dialect, val naming: Naming, val session: MirrorSession = MirrorSession("DefaultMirrorContextSession"))
    extends MirrorContextBase[Dialect, Naming] with AstSplicing {

  export MirrorContext.{
    PrepareRow => _,
    ResultRow => _,
    Session => _,
    _
  }
}

trait MirrorContextBase[+Dialect <: Idiom, +Naming <: NamingStrategy]
    extends Context[Dialect, Naming]
    with ContextVerbPrepare[Dialect, Naming]
    with ContextVerbTranslate[Dialect, Naming] { self =>

  override type Result[T] = T
  override type RunQueryResult[T] = QueryMirror[T]
  override type RunQuerySingleResult[T] = QueryMirror[T]
  override type PrepareRow = Row
  override type ResultRow = Row
  override type RunActionResult = ActionMirror
  override type RunActionReturningResult[T] = ActionReturningMirror[_, T]
  override type RunBatchActionReturningResult[T] = BatchActionReturningMirror[T]
  override type RunBatchActionResult = BatchActionMirror
  override type Session = MirrorSession
  override type Extractor[T] = (ResultRow, MirrorSession) => T

  override type Runner = Unit
  override type TranslateRunner = Unit
  override def context: Runner = ()
  override def translateContext: Runner = ()
  def session: MirrorSession



  case class QueryMirror[T](string: String, prepareRow: PrepareRow, extractor: Extractor[T], info: ExecutionInfo) {
    def string(pretty: Boolean): String =
      if (pretty)
        idiom.format(string)
      else
        string
  }

  case class ActionMirror(string: String, prepareRow: PrepareRow, info: ExecutionInfo)
  case class ActionReturningMirror[T, R](string: String, prepareRow: PrepareRow, extractor: Extractor[T], returningBehavior: ReturnAction, info: ExecutionInfo)
  case class BatchActionMirror(groups: List[(String, List[Row])], info: ExecutionInfo)
  case class BatchActionReturningMirror[T](groups: List[(String, ReturnAction, List[PrepareRow])], extractor: Extractor[T], info: ExecutionInfo)

  @targetName("runQueryDefault")
  inline def run[T](inline quoted: Quoted[Query[T]]): QueryMirror[T] = InternalApi.runQueryDefault(quoted)
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): QueryMirror[T] = InternalApi.runQuery(quoted, wrap)
  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): QueryMirror[T] = InternalApi.runQuerySingle(quoted)
  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): ActionMirror = InternalApi.runAction(quoted)
  @targetName("runActionReturning")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, T]]): ActionReturningMirror[T, T] = InternalApi.runActionReturning(quoted).asInstanceOf[ActionReturningMirror[T, T]]
  @targetName("runActionReturningMany")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, List[T]]]): ActionReturningMirror[T, List[T]] = InternalApi.runActionReturningMany(quoted).asInstanceOf[ActionReturningMirror[T, List[T]]]
  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]], rowsPerBatch: Int): BatchActionMirror = InternalApi.runBatchAction(quoted, rowsPerBatch)
  @targetName("runBatchActionDefault")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): BatchActionMirror = InternalApi.runBatchAction(quoted, 1)
  @targetName("runBatchActionReturning")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]], rowsPerBatch: Int): BatchActionReturningMirror[T] = InternalApi.runBatchActionReturning(quoted, rowsPerBatch)
  @targetName("runBatchActionReturningDefault")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): BatchActionReturningMirror[T] = InternalApi.runBatchActionReturning(quoted, 1)

  override def executeQuery[T](string: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner) =
    QueryMirror(string, prepare(Row(), session)._2, extractor, info)

  override def executeQuerySingle[T](string: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner) =
    QueryMirror(string, prepare(Row(), session)._2, extractor, info)

  override def executeAction(string: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner): Result[RunActionResult] =
    ActionMirror(string, prepare(Row(), session)._2, info)

  def executeActionReturning[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner): Result[RunActionReturningResult[T]] =
    ActionReturningMirror[T, T](sql, prepare(Row(), session)._2, extractor, returningBehavior, info)

  def executeActionReturningMany[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner): Result[RunActionReturningResult[List[T]]] =
    ActionReturningMirror[T, List[T]](sql, prepare(Row(), session)._2, extractor, returningBehavior, info)

  override def executeBatchAction(groups: List[BatchGroup])(info: ExecutionInfo, dc: Runner): Result[RunBatchActionResult] =
    BatchActionMirror(
      groups.map {
        case BatchGroup(string, prepare) =>
          (string, prepare.map(_(Row(), session)._2))
      },
      info
    )

  override def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(info: ExecutionInfo, dc: Runner): Result[RunBatchActionReturningResult[T]] =
    BatchActionReturningMirror[T](
      groups.map {
        case BatchGroupReturning(string, returningBehavior, prepare) =>
          (string, returningBehavior, prepare.map(_(Row(), session)._2))
      },
      extractor,
      info
    )

  case class PrepareQueryMirror(sql: String, prepare: Prepare, info: ExecutionInfo)
  case class PrepareBatchMirror(groups: List[(String, List[PrepareRow])], info: ExecutionInfo)

  type PrepareQueryResult = PrepareQueryMirror
  type PrepareActionResult = PrepareQueryMirror
  type PrepareBatchActionResult = PrepareBatchMirror

  def prepareSingle(string: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner) =
    PrepareQueryMirror(string, prepare, info)

  def prepareQuery(string: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner) =
    prepareSingle(string, prepare)(info, dc)

  def prepareAction(string: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner) =
    prepareSingle(string, prepare)(info, dc)

  def prepareBatchAction(groups: List[BatchGroup])(info: ExecutionInfo, dc: Runner) =
    PrepareBatchMirror(
      groups.map {
        case BatchGroup(string, prepare) =>
          (string, prepare.map(_(Row(), session)._2))
      },
      info
    )

  override private[getquill] def prepareParams(statement: String, prepare: Prepare): Seq[String] = {
    val prepData = prepare(Row(), session)._2.data.map(_._2)
    prepData.map(prepareParam)
  }
}
