package zio.quill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import zio.quill.{ReturnAction}
import zio.quill.generic.EncodingDsl
import zio.quill.Quoted
import zio.quill.QueryMeta
import zio.quill.generic._
import zio.quill.context.mirror.MirrorDecoders
import zio.quill.context.mirror.Row
import zio.quill.generic.GenericDecoder
import zio.quill.generic.DecodingType
import zio.quill.Planter
import zio.quill.ast.Ast
import zio.quill.ast.ScalarTag
import scala.quoted._
import zio.quill.idiom.Idiom
import zio.quill.ast.{Transform, QuotationTag}
import zio.quill.QuotationLot
import zio.quill.metaprog.QuotedExpr
import zio.quill.metaprog.PlanterExpr
import zio.quill.idiom.ReifyStatement
import zio.quill.Query
import zio.quill.metaprog.etc.MapFlicer
import zio.quill.util.Messages.fail
import java.io.Closeable
import zio.quill.util.Format
import zio.quill.QAC
import zio.quill.Action
import zio.quill.ActionReturning
import zio.quill.BatchAction
import zio.quill.Literal
import scala.annotation.targetName
import zio.quill.NamingStrategy
import zio.quill.idiom.Idiom
import zio.quill.context.ProtoContextSecundus
import zio.quill.context.AstSplicing
import zio.quill.context.RowContext
import zio.quill.metaprog.etc.ColumnsFlicer
import zio.quill.context.Execution.ElaborationBehavior
import zio.quill.OuterSelectWrap

trait ContextVerbPrepare[+Dialect <: Idiom, +Naming <: NamingStrategy] {
  self: Context[Dialect, Naming] =>

  type Result[T]
  type Session
  type Runner

  type PrepareQueryResult // Usually: Session => Result[PrepareRow]
  type PrepareActionResult // Usually: Session => Result[PrepareRow]
  type PrepareBatchActionResult // Usually: Session => Result[List[PrepareRow]]

  def prepareQuery(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): PrepareQueryResult
  def prepareSingle(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): PrepareQueryResult
  def prepareAction(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: Runner): PrepareActionResult
  def prepareBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, dc: Runner): PrepareBatchActionResult

  // Summon a implicit execution context if needed (e.g. in Jasync contexts)
  inline def _summonPrepareRunner() = DatasourceContextInjectionMacro[RunnerBehavior, Runner, this.type](context)

  // Must be lazy since idiom/naming are null (in some contexts) initially due to initialization order
  private lazy val make = ContextOperation.Factory[Dialect, Naming, PrepareRow, ResultRow, Session, this.type](self.idiom, self.naming)

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
  inline def prepare[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): PrepareBatchActionResult = {
    val ca = make.batch[I, Nothing, A, PrepareBatchActionResult] { arg =>
      val groups = arg.groups.map((sql, prepare) => BatchGroup(sql, prepare, Nil))
      self.prepareBatchAction(groups.toList)(arg.executionInfo, _summonPrepareRunner())
    }
    QueryExecutionBatch.apply(ca, 1)(quoted)
  }
} // end ContextVerbPrepare
