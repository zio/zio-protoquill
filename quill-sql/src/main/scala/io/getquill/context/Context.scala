package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ReturnAction}
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
import java.awt.Dialog
import org.scalafmt.config.ScalafmtRunner.Dialect

sealed trait ExecutionType
object ExecutionType:
  case object Dynamic extends ExecutionType
  case object Static extends ExecutionType

/** Metadata related to query execution. Note that AST should be lazy so as not to be evaluated at runtime (which would happen with a by-value property since `{
  * ExecutionInfo(stuff, ast) } is spliced into a query-execution site) TODO As a future optimization (if needed) could we introduce a compiler argument that would not even splice
  * the ASTs during the execute___ call-sites in the Context?
  */
class ExecutionInfo(val executionType: ExecutionType, queryAst: => Ast):
  def ast: Ast = queryAst
object ExecutionInfo:
  def apply(executionType: ExecutionType, ast: => Ast) = new ExecutionInfo(executionType, ast)

trait ProtoStreamContext[Dialect <: Idiom, Naming <: NamingStrategy] extends RowContext {
  type PrepareRow
  type ResultRow

  type DatasourceContext
  type StreamResult[T]

  def streamQuery[T](fetchSize: Option[Int], sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(
      executionInfo: ExecutionInfo,
      dc: DatasourceContext
  ): StreamResult[T]
}

trait RowContext {
  type PrepareRow
  type ResultRow

  protected val identityPrepare: Prepare = (p: PrepareRow, s: Session) => (Nil, p)
  protected val identityExtractor = (rr: ResultRow, s: Session) => rr

  case class BatchGroup(string: String, prepare: List[Prepare])
  case class BatchGroupReturning(string: String, returningBehavior: ReturnAction, prepare: List[Prepare])

  type Prepare = (PrepareRow, Session) => (List[Any], PrepareRow)
  type Extractor[T] = (ResultRow, Session) => T
  type Session
}

trait ProtoContext[Dialect <: Idiom, Naming <: NamingStrategy] extends RowContext {
  type PrepareRow
  type ResultRow

  type Result[T]
  type RunQuerySingleResult[T]
  type RunQueryResult[T]
  type RunActionResult
  type RunActionReturningResult[T]
  type RunBatchActionResult
  type RunBatchActionReturningResult[T]
  type Session

  /** Future class to hold things like ExecutionContext for Cassandra etc... */
  type DatasourceContext

  def idiom: Dialect
  def naming: Naming

  def executeQuery[T](sql: String, prepare: Prepare, extractor: Extractor[T])(executionInfo: ExecutionInfo, dc: DatasourceContext): Result[RunQueryResult[T]]
  def executeQuerySingle[T](string: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(
      executionInfo: ExecutionInfo,
      dc: DatasourceContext
  ): Result[RunQuerySingleResult[T]]
  def executeAction[T](sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: DatasourceContext): Result[RunActionResult]
  def executeActionReturning[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T], returningBehavior: ReturnAction)(
      executionInfo: ExecutionInfo,
      dc: DatasourceContext
  ): Result[RunActionReturningResult[T]]
  def executeBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, dc: DatasourceContext): Result[RunBatchActionResult]
  def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(
      executionInfo: ExecutionInfo,
      dc: DatasourceContext
  ): Result[RunBatchActionReturningResult[T]]
}

trait AstSplicing

sealed trait DatasourceContextInjection
object DatasourceContextInjection {
  sealed trait Implicit extends DatasourceContextInjection
  object Implicit extends Implicit
  sealed trait Member extends DatasourceContextInjection
  object Member extends Member
}

sealed trait Extraction[-ResultRow, -Session, +T]
object Extraction {
  case class Simple[ResultRow, Session, T](extract: (ResultRow, Session) => T) extends Extraction[ResultRow, Session, T]
  case class Returning[ResultRow, Session, T](extract: (ResultRow, Session) => T, returningBehavior: ReturnAction) extends Extraction[ResultRow, Session, T]
  case object None extends Extraction[Any, Any, Nothing]
}

import io.getquill.generic.DecodeAlternate

// TODO Needs to be portable (i.e. plug into current contexts when compiled with Scala 3)
trait Context[Dialect <: Idiom, Naming <: NamingStrategy] extends ProtoContext[Dialect, Naming] with EncodingDsl with Closeable { self =>

  type DatasourceContextBehavior <: DatasourceContextInjection

  // TODO Go back to this when implementing GenericDecoder using standard method
  //implicit inline def autoDecoder[T]: BaseDecoder[T] = GenericDecoder.generic
  // inline given dec[T](using dec: DecodeAlternate[T, ResultRow]): GenericDecoder[T, ResultRow] with
  //   inline def decode(t: T) = ${ DecodeAlternate[T, ResultRow] }

  implicit inline def dec[T]: GenericDecoder[ResultRow, Session, T, DecodingType.Generic] = ${ GenericDecoder.summon[T, ResultRow, Session] }

  //def probe(statement: String): Try[_]
  // todo add 'prepare' i.e. encoders here
  //def executeAction[T](cql: String, prepare: Prepare = identityPrepare)(implicit executionContext: ExecutionContext): Result[RunActionResult]

  inline def lift[T](inline runtimeValue: T): T =
    ${ LiftMacro[T, PrepareRow, Session]('runtimeValue) } // Needs PrepareRow in order to be able to summon encoders

  inline def liftQuery[U[_] <: Iterable[_], T](inline runtimeValue: U[T]): Query[T] =
    ${ LiftQueryMacro[T, U, PrepareRow, Session]('runtimeValue) }

  extension [T](inline q: Query[T]) {
    inline def filterByKeys(inline map: Map[String, String]) =
      q.filter(p => MapFlicer[T, PrepareRow, Session](p, map, null, (a, b) => (a == b) || (b == (null))))
  }

  protected def context: DatasourceContext = fail(s"DatasourceContext method not implemented for '${this.getClass}' Context")

  // Think I need to implement 'run' here as opposed to in Context because an abstract
  // inline method cannot be called. Should look into this further. E.g. maybe the 'inline' in
  // the regular context can be non inline
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val ca = new ContextOperation[Nothing, T, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, Result[RunQueryResult[T]]](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, T],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        val extract = extraction match
          case Extraction.Simple(extract) => extract
          case _                          => throw new IllegalArgumentException("Extractor required")

        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.executeQuery(sql, prepare, extract)(executionInfo, runContext)
    }
    // TODO Could make Quoted operation constructor that is a typeclass, not really necessary though
    QueryExecution.apply(quoted, ca, None)
  }

  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): Result[RunQuerySingleResult[T]] = {
    val ca = new ContextOperation[Nothing, T, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, Result[RunQuerySingleResult[T]]](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, T],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        val extract = extraction match
          case Extraction.Simple(extract) => extract
          case _                          => throw new IllegalArgumentException("Extractor required")

        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.executeQuerySingle(sql, prepare, extract)(executionInfo, runContext)
    }
    // TODO Could make Quoted operation constructor that is a typeclass, not really necessary though
    QueryExecution.apply(QuerySingleAsQuery(quoted), ca, None)
  }

  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): Result[RunActionResult] = {
    val ca = new ContextOperation[E, Any, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, Result[RunActionResult]](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, Any],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.executeAction(sql, prepare)(executionInfo, runContext)
    }
    QueryExecution.apply(quoted, ca, None)
  }

  @targetName("runActionReturning")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, T]]): Result[RunActionReturningResult[T]] = {
    val ca = new ContextOperation[E, T, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, Result[RunActionReturningResult[T]]](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, T],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        // Need an extractor with special information that helps with the SQL returning specifics
        val Extraction.Returning(extract, returningBehavior) =
          // Just match on the type and throw an exception. The outside val right above will do the deconstruction
          extraction match
            // Can't check types inside Returning[_, _] during runtime due to type-erasure so scala will give a warning
            case _: Extraction.Returning[_, _, _] => extraction
            case _                                => throw new IllegalArgumentException("Returning Extractor required")

        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.executeActionReturning(sql, prepare, extract, returningBehavior)(executionInfo, runContext)
    }
    QueryExecution.apply(quoted, ca, None)
  }

  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): Result[RunBatchActionResult] = {
    val ca = new BatchContextOperation[I, Nothing, A, Dialect, Naming, PrepareRow, ResultRow, Session, Result[RunBatchActionResult]](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepares: List[(PrepareRow, Session) => (List[Any], PrepareRow)],
          extraction: Extraction[ResultRow, Session, Nothing],
          executionInfo: ExecutionInfo
      ) =
        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
        val group = BatchGroup(sql, prepares)
        self.executeBatchAction(List(group))(executionInfo, runContext)
    }
    BatchQueryExecution.apply(quoted, ca)
  }

  @targetName("runBatchActionReturning")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): Result[RunBatchActionReturningResult[T]] = {
    val ca = new BatchContextOperation[I, T, A, Dialect, Naming, PrepareRow, ResultRow, Session, Result[RunBatchActionReturningResult[T]]](self.idiom, self.naming) {
      def execute(sql: String, prepares: List[(PrepareRow, Session) => (List[Any], PrepareRow)], extraction: Extraction[ResultRow, Session, T], executionInfo: ExecutionInfo) =
        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)

        val Extraction.Returning(extract, returningBehavior) =
          extraction match
            case _: Extraction.Returning[_, _, _] => extraction
            case _                                => throw new IllegalArgumentException("Returning Extractor required")

        // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
        val group = BatchGroupReturning(sql, returningBehavior, prepares)
        self.executeBatchActionReturning[T](List(group), extract)(executionInfo, runContext)
    }
    BatchQueryExecution.apply(quoted, ca)
  }

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => fail(s"Expected a single result but got $other")
    }

  // Can close context. Does nothing by default.
  def close(): Unit = ()
}

trait StreamingContext[Dialect <: io.getquill.idiom.Idiom, Naming <: NamingStrategy] extends ProtoStreamContext[Dialect, Naming] {
  self: Context[Dialect, Naming] =>

  @targetName("streamQuery")
  inline def stream[T](inline quoted: Quoted[Query[T]]): StreamResult[T] = {
    val ca = new ContextOperation[Nothing, T, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, StreamResult[T]](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, T],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        val extract = extraction match
          case Extraction.Simple(extract) => extract
          case _                          => throw new IllegalArgumentException("Extractor required")

        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.streamQuery(fetchSize, sql, prepare, extract)(executionInfo, runContext)
    }
    // TODO Could make Quoted operation constructor that is a typeclass, not really necessary though
    QueryExecution.apply(quoted, ca, None)
  }

  @targetName("streamQueryWithFetchSize")
  inline def stream[T](inline quoted: Quoted[Query[T]], fetchSize: Int): StreamResult[T] = {
    val ca = new ContextOperation[Nothing, T, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, StreamResult[T]](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, T],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        val extract = extraction match
          case Extraction.Simple(extract) => extract
          case _                          => throw new IllegalArgumentException("Extractor required")

        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.streamQuery(fetchSize, sql, prepare, extract)(executionInfo, runContext)
    }
    // TODO Could make Quoted operation constructor that is a typeclass, not really necessary though
    QueryExecution.apply(quoted, ca, Some(fetchSize))
  }
}

trait PrepareContext[Dialect <: Idiom, Naming <: NamingStrategy] {
  self: Context[Dialect, Naming] =>

  type Result[T]
  type Session
  type DatasourceContext

  type PrepareQueryResult //Usually: Session => Result[PrepareRow]
  type PrepareActionResult //Usually: Session => Result[PrepareRow]
  type PrepareBatchActionResult //Usually: Session => Result[List[PrepareRow]]

  def prepareQuery(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: DatasourceContext): PrepareQueryResult
  def prepareSingle(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: DatasourceContext): PrepareQueryResult
  def prepareAction(sql: String, prepare: Prepare = identityPrepare)(executionInfo: ExecutionInfo, dc: DatasourceContext): PrepareActionResult
  def prepareBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, dc: DatasourceContext): PrepareBatchActionResult

  @targetName("runPrepareQuery")
  inline def prepare[T](inline quoted: Quoted[Query[T]]): PrepareQueryResult = {
    val ca = new ContextOperation[Nothing, T, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, PrepareQueryResult](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, T],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        val extract = extraction match
          case Extraction.Simple(extract) => extract
          case _                          => throw new IllegalArgumentException("Extractor required")

        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.prepareQuery(sql, prepare)(executionInfo, runContext)
    }
    QueryExecution.apply(quoted, ca, None)
  }

  @targetName("runPrepareQuerySingle")
  inline def prepare[T](inline quoted: Quoted[T]): PrepareQueryResult = prepare(QuerySingleAsQuery(quoted))

  @targetName("runPrepareAction")
  inline def prepare[E](inline quoted: Quoted[Action[E]]): PrepareActionResult = {
    val ca = new ContextOperation[E, Any, Dialect, Naming, PrepareRow, ResultRow, Session, this.type, PrepareActionResult](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
          extraction: Extraction[ResultRow, Session, Any],
          executionInfo: ExecutionInfo,
          fetchSize: Option[Int]
      ) =
        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.prepareAction(sql, prepare)(executionInfo, runContext)
    }
    QueryExecution.apply(quoted, ca, None)
  }

  @targetName("runPrepareBatchAction")
  inline def prepare[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): PrepareBatchActionResult = {
    val ca = new BatchContextOperation[I, Nothing, A, Dialect, Naming, PrepareRow, ResultRow, Session, PrepareBatchActionResult](self.idiom, self.naming) {
      def execute(
          sql: String,
          prepares: List[(PrepareRow, Session) => (List[Any], PrepareRow)],
          extraction: Extraction[ResultRow, Session, Nothing],
          executionInfo: ExecutionInfo
      ) =
        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        val group = BatchGroup(sql, prepares)
        self.prepareBatchAction(List(group))(executionInfo, runContext)
    }
    BatchQueryExecution.apply(quoted, ca)
  }
}

trait StagedPrepare[Dialect <: Idiom, Naming <: NamingStrategy] extends PrepareContext[Dialect, Naming] {
  self: Context[Dialect, Naming] =>

  type PrepareQueryResult = Session => Result[PrepareRow]
  type PrepareActionResult = Session => Result[PrepareRow]
  type PrepareBatchActionResult = Session => Result[List[PrepareRow]]
}

trait StandardContext[Idiom <: io.getquill.idiom.Idiom, Naming <: NamingStrategy]
    extends Context[Idiom, Naming]
    with StagedPrepare[Idiom, Naming]
