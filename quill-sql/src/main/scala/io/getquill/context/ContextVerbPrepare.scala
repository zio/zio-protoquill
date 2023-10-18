package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.ReturnAction
import io.getquill.generic.EncodingDsl
import io.getquill.Quoted
import io.getquill.QueryMeta
import io.getquill.generic._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import scala.quoted._
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.Query
import io.getquill.metaprog.etc.MapFlicer
import io.getquill.util.Messages.fail
import java.io.Closeable
import io.getquill.util.Format
import io.getquill.QAC
import io.getquill.Action
import io.getquill.ActionReturning
import io.getquill.BatchAction
import io.getquill.Literal
import scala.annotation.targetName
import io.getquill.NamingStrategy
import io.getquill.idiom.Idiom
import io.getquill.context.ProtoContextSecundus
import io.getquill.context.AstSplicing
import io.getquill.context.RowContext
import io.getquill.metaprog.etc.ColumnsFlicer
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.OuterSelectWrap

trait ContextVerbPrepare[+Dialect <: Idiom, +Naming <: NamingStrategy] {
  self: Context[Dialect, Naming] =>

  type Result[T]
  type Session
  type Runner

  type PrepareQueryResult       // Usually: Session => Result[PrepareRow]
  type PrepareActionResult      // Usually: Session => Result[PrepareRow]
  type PrepareBatchActionResult // Usually: Session => Result[List[PrepareRow]]

  def prepareQuery(sql: String, prepare: Prepare = identityPrepare)(
    executionInfo: ExecutionInfo,
    dc: Runner
  ): PrepareQueryResult
  def prepareSingle(sql: String, prepare: Prepare = identityPrepare)(
    executionInfo: ExecutionInfo,
    dc: Runner
  ): PrepareQueryResult
  def prepareAction(sql: String, prepare: Prepare = identityPrepare)(
    executionInfo: ExecutionInfo,
    dc: Runner
  ): PrepareActionResult
  def prepareBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, dc: Runner): PrepareBatchActionResult

  // Summon a implicit execution context if needed (e.g. in Jasync contexts)
  inline def _summonPrepareRunner() = DatasourceContextInjectionMacro[RunnerBehavior, Runner, this.type](context)

  // Must be lazy since idiom/naming are null (in some contexts) initially due to initialization order
  private lazy val make =
    ContextOperation.Factory[Dialect, Naming, PrepareRow, ResultRow, Session, this.type](self.idiom, self.naming)

  @targetName("runPrepareQuery")
  inline def prepare[T](inline quoted: Quoted[Query[T]]): PrepareQueryResult = {
    val ca = make.op[Nothing, T, PrepareQueryResult] { arg =>
      self.prepareQuery(arg.sql, arg.prepare)(arg.executionInfo, _summonPrepareRunner())
    }
    QueryExecution.apply(ca)(quoted, None)
  }

  @targetName("runPrepareQuerySingle")
  inline def prepare[T](inline quoted: Quoted[T]): PrepareQueryResult = prepare(QuerySingleAsQuery(quoted))

  @targetName("runPrepareAction")
  inline def prepare[E](inline quoted: Quoted[Action[E]]): PrepareActionResult = {
    val ca = make.op[E, Any, PrepareActionResult] { arg =>
      self.prepareAction(arg.sql, arg.prepare)(arg.executionInfo, _summonPrepareRunner())
    }
    QueryExecution.apply(ca)(quoted, None)
  }

  @targetName("runPrepareBatchAction")
  inline def prepare[I, A <: Action[I] & QAC[I, Nothing]](
    inline quoted: Quoted[BatchAction[A]]
  ): PrepareBatchActionResult = {
    val ca = make.batch[I, Nothing, A, PrepareBatchActionResult] { arg =>
      val groups = arg.groups.map((sql, prepare) => BatchGroup(sql, prepare))
      self.prepareBatchAction(groups.toList)(arg.executionInfo, _summonPrepareRunner())
    }
    QueryExecutionBatch.apply(ca, 1)(quoted)
  }
} // end ContextVerbPrepare
