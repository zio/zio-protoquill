package io.getquill

import io.getquill.context.mirror._
import io.getquill.context._
import io.getquill.Quoted
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy

sealed trait Dummy
object DummyInst extends Dummy

// TODO Dialect <: Idiom, Naming <: NamingStrategy are not actually needed so can remove them but that causes a compile-time error. Need to figure that out
// TODO Modify some major unit tests with embedded etc... to make sure this works in all cases
trait MirrorColumnResolving[Dialect <: Idiom, Naming <: NamingStrategy] { self: MirrorContextBase[Dialect, Naming] =>
  given mirrorResover: ColumnResolver with {
    def apply(resultRow: Row, columnName: String): Int = resultRow.indexOfKey(columnName)
  }
}

case class MirrorSession(name: String)
object MirrorSession {
  def default = MirrorSession("DefaultMirrorSession")
}

class MirrorContext[Dialect <: Idiom, Naming <: NamingStrategy](val idiom: Dialect, val naming: Naming, val session: MirrorSession = MirrorSession("DefaultMirrorContextSession"))
extends MirrorContextBase[Dialect, Naming] with AstSplicing

trait MirrorContextBase[Dialect <: Idiom, Naming <: NamingStrategy]
extends Context[Dialect, Naming]
with PrepareContext[Dialect, Naming]
with MirrorDecoders
with MirrorEncoders { self =>
  override type Result[T] = T
  override type RunQueryResult[T] = QueryMirror[T]
  override type RunQuerySingleResult[T] = QueryMirror[T]
  override type PrepareRow = Row
  override type ResultRow = Row
  override type RunActionResult = ActionMirror
  override type RunActionReturningResult[T] = ActionReturningMirror[T]
  override type RunBatchActionReturningResult[T] = BatchActionReturningMirror[T]
  override type RunBatchActionResult = BatchActionMirror
  override type Session = MirrorSession

  override type Runner = Unit
  override def context: Runner = ()
  def session: MirrorSession

  // TODO Not needed, get rid of this
  implicit val d: Dummy = DummyInst

  case class QueryMirror[T](string: String, prepareRow: PrepareRow, extractor: Extractor[T], info: ExecutionInfo) {
    def string(pretty: Boolean): String =
      if (pretty)
        idiom.format(string)
      else
        string
  }

  case class ActionMirror(string: String, prepareRow: PrepareRow, info: ExecutionInfo)
  case class ActionReturningMirror[T](string: String, prepareRow: PrepareRow, extractor: Extractor[T], returningBehavior: ReturnAction, info: ExecutionInfo)
  case class BatchActionMirror(groups: List[(String, List[Row])], info: ExecutionInfo)
  case class BatchActionReturningMirror[T](groups: List[(String, ReturnAction, List[PrepareRow])], extractor: Extractor[T], info: ExecutionInfo)

  override def executeQuery[T](string: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner) =
    QueryMirror(string, prepare(Row(), session)._2, extractor, info)

  override def executeQuerySingle[T](string: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner) =
    QueryMirror(string, prepare(Row(), session)._2, extractor, info)

  override def executeAction[T](string: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner): Result[RunActionResult] =
    ActionMirror(string, prepare(Row(), session)._2, info)

  def executeActionReturning[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner): Result[RunActionReturningResult[T]] =
    ActionReturningMirror[T](sql, prepare(Row(), session)._2, extractor, returningBehavior, info)

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
      }, extractor,
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
}
