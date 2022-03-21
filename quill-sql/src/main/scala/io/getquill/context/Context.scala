package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
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
import io.getquill.context.ProtoContext
import io.getquill.context.AstSplicing
import io.getquill.context.RowContext
import io.getquill.metaprog.etc.ColumnsFlicer
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.OuterSelectWrap

/**
 * Metadata related to query execution. Note that AST should be lazy so as not to be evaluated
 * at runtime (which would happen with a by-value property since `{ ExecutionInfo(stuff, ast) } is spliced
 * into a query-execution site)
 * TODO As a future optimization (if needed) could we introduce a compiler argument that would not even
 * splice the ASTs during the execute___ call-sites in the Context?
 */
class ExecutionInfo(val executionType: ExecutionType, queryAst: =>Ast):
  def ast: Ast = queryAst
object ExecutionInfo:
  def apply(executionType: ExecutionType, ast: =>Ast) = new ExecutionInfo(executionType, ast)

trait ProtoStreamContext[Dialect <: Idiom, Naming <: NamingStrategy] extends RowContext {
  type PrepareRow
  type ResultRow

  type Runner
  type StreamResult[T]

  def streamQuery[T](fetchSize: Option[Int], sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(executionInfo: ExecutionInfo, dc: Runner): StreamResult[T]
}

sealed trait RunnerSummoningBehavior
object RunnerSummoningBehavior {
  sealed trait Implicit extends RunnerSummoningBehavior
  object Implicit extends Implicit
  sealed trait Member extends RunnerSummoningBehavior
  object Member extends Member
}

sealed trait Extraction[-ResultRow, -Session, +T]:
  /** Require an effect to be be simple and retrieve it. Effectful at compile-time since it can fail compilation */
  def requireSimple() =
    this match
      case ext: Extraction.Simple[_, _, _] => ext
      case _ => throw new IllegalArgumentException("Extractor required")
  /** Require an effect to be be returning and retrieve it. Effectful at compile-time since it can fail compilation */
  def requireReturning() =
    this match
      case ext: Extraction.Returning[_, _, _] => ext
      case _ => throw new IllegalArgumentException("Returning Extractor required")


object Extraction:
  case class Simple[ResultRow, Session, T](extract: (ResultRow, Session) => T) extends Extraction[ResultRow, Session, T]
  case class Returning[ResultRow, Session, T](extract: (ResultRow, Session) => T, returningBehavior: ReturnAction) extends Extraction[ResultRow, Session, T]
  case object None extends Extraction[Any, Any, Nothing]

import io.getquill.generic.DecodeAlternate

trait ContextStandard[Idiom <: io.getquill.idiom.Idiom, Naming <: NamingStrategy]
  extends Context[Idiom, Naming]
  with ContextVerbPrepareLamba[Idiom, Naming]


trait Context[Dialect <: Idiom, Naming <: NamingStrategy]
  extends ProtoContext[Dialect, Naming] with EncodingDsl with Closeable:
  self =>

  /**
    * Base type used to determine whether there is an execution context that needs to be summoned to perform
    * execution methods e.g. in the PostgresJasync contexts that use Scala Futures that need an ExecutionContext.
    */
  type RunnerBehavior <: RunnerSummoningBehavior
  protected def context: Runner = fail(s"Runner method not implemented for '${this.getClass.getName}' Context")

  implicit inline def dec[T]: GenericDecoder[ResultRow, Session, T, DecodingType.Generic] = ${ GenericDecoder.summon[T, ResultRow, Session] }

  //def probe(statement: String): Try[_]
  // todo add 'prepare' i.e. encoders here
  //def executeAction(cql: String, prepare: Prepare = identityPrepare)(implicit executionContext: ExecutionContext): Result[RunActionResult]

  inline def lift[T](inline runtimeValue: T): T =
    ${ LiftMacro[T, PrepareRow, Session]('runtimeValue) } // Needs PrepareRow in order to be able to summon encoders

  inline def liftQuery[U[_] <: Iterable[_], T](inline runtimeValue: U[T]): Query[T] =
    ${ LiftQueryMacro[T, U, PrepareRow, Session]('runtimeValue) }

  extension [T](inline q: Query[T]) {
    /**
     * When using this with FilterColumns make sure it comes FIRST. Otherwise the columns are you filtering
     * may have been nullified in the SQL before the filteration has actually happened.
     */
    inline def filterByKeys(inline map: Map[String, String]) =
      q.filter(p => MapFlicer[T, PrepareRow, Session](p, map, null, (a, b) => (a == b) || (b == (null) ) ))

    inline def filterColumns(inline columns: List[String]) =
      q.map(p => ColumnsFlicer[T, PrepareRow, Session](p, columns))
  }

  /*
   * In previous version of ProtoQuill there was no InternalApi and the `run` methods were used
   * on a context directly. This caused a problem where doing something like:
   *   val list: List[RightType] = run(query[WrongType])
   * Will yield:
   *   Found: ctx.Result[List[WrongType]] Required: List[RightType]
   *
   * This was even worse if the types were more complex e.g. for ZIO:
   *   val list: ZIO[Has[DataSource], SQLException, List[RightType]] = run(query[WrongType])
   *
   * Will yield:
   *   Found: QuillContext.Result[List[WrongType]] Required: ZIO[Has[DataSource], SQLException, List[RightType]]
   *
   * This is quite confusing. Therefore we define the methods in an object and then
   * delegate to these in the individual contexts.
   */
  object InternalApi:
    /** Internal API that cannot be made private due to how inline functions */
    inline def _summonRunner() = DatasourceContextInjectionMacro[RunnerBehavior, Runner, self.type](context)

    inline def runQueryDefault[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] =
      runQuery(quoted, OuterSelectWrap.Default)

    // Must be lazy since idiom/naming are null (in some contexts) initially due to initialization order
    private lazy val make = ContextOperation.Factory[Dialect, Naming, PrepareRow, ResultRow, Session, self.type](self.idiom, self.naming)

    inline def runQuery[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): Result[RunQueryResult[T]] = {
      val ca = make.op[Nothing, T, Result[RunQueryResult[T]]] { arg =>
        val simpleExt = arg.extractor.requireSimple()
        self.executeQuery(arg.sql, arg.prepare.head, simpleExt.extract)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(quoted, ca, None, wrap)
    }

    inline def runQuerySingle[T](inline quoted: Quoted[T]): Result[RunQuerySingleResult[T]] = {
      val ca = make.op[Nothing, T, Result[RunQuerySingleResult[T]]] { arg =>
        val simpleExt = arg.extractor.requireSimple()
        self.executeQuerySingle(arg.sql, arg.prepare.head, simpleExt.extract)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(QuerySingleAsQuery(quoted), ca, None)
    }

    inline def runAction[E](inline quoted: Quoted[Action[E]]): Result[RunActionResult] = {
      val ca = make.op[E, Any, Result[RunActionResult]] { arg =>
        self.executeAction(arg.sql, arg.prepare.head)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(quoted, ca, None)
    }

    inline def runActionReturning[E, T](inline quoted: Quoted[ActionReturning[E, T]]): Result[RunActionReturningResult[T]] = {
      val ca = make.op[E, T, Result[RunActionReturningResult[T]]] { arg =>
        // Need an extractor with special information that helps with the SQL returning specifics
        val returningExt = arg.extractor.requireReturning()
        self.executeActionReturning(arg.sql, arg.prepare.head, returningExt.extract, returningExt.returningBehavior)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(quoted, ca, None)
    }

    inline def runBatchAction[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): Result[RunBatchActionResult] = {
      val ca = make.batch[I, Nothing, A, Result[RunBatchActionResult]] { arg =>
        // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
        val group = BatchGroup(arg.sql, arg.prepare.toList)
        self.executeBatchAction(List(group))(arg.executionInfo, _summonRunner())
      }
      BatchQueryExecution.apply(quoted, ca)
    }

    inline def runBatchActionReturning[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): Result[RunBatchActionReturningResult[T]] = {
      val ca = make.batch[I, T, A, Result[RunBatchActionReturningResult[T]]] { arg =>
        val returningExt = arg.extractor.requireReturning()
        // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
        val group = BatchGroupReturning(arg.sql, returningExt.returningBehavior, arg.prepare.toList)
        self.executeBatchActionReturning[T](List(group), returningExt.extract)(arg.executionInfo, _summonRunner())
      }
      BatchQueryExecution.apply(quoted, ca)
    }
  end InternalApi

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => fail(s"Expected a single result but got $other")
    }

  // Can close context. Does nothing by default.
  def close(): Unit = ()
end Context
