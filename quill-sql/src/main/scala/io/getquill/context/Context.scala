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
import QueryExecution.QuotedOperation
import io.getquill.metaprog.etc.MapFlicer
import io.getquill.util.Messages.fail
import java.io.Closeable
import io.getquill.util.Format

import io.getquill._

sealed trait ExecutionType
object ExecutionType {
  case object Dynamic extends ExecutionType
  case object Static extends ExecutionType
}

trait ProtoContext[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy] {
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

  type Prepare = PrepareRow => (List[Any], PrepareRow)
  type Extractor[T] = ResultRow => T

  case class BatchGroup(string: String, prepare: List[Prepare])
  case class BatchGroupReturning(string: String, returningBehavior: ReturnAction, prepare: List[Prepare])

  def idiom: Dialect
  def naming: Naming
  
  val identityPrepare: Prepare = (Nil, _)
  val identityExtractor = identity[ResultRow] _

  def executeQuery[T](sql: String, prepare: Prepare, extractor: Extractor[T])(executionType: ExecutionType, dc: DatasourceContext): Result[RunQueryResult[T]]
  def executeAction[T](sql: String, prepare: Prepare = identityPrepare)(executionType: ExecutionType, dc: DatasourceContext): Result[RunActionResult]
  def executeBatchAction(groups: List[BatchGroup])(executionType: ExecutionType, dc: DatasourceContext): Result[RunBatchActionResult]

  // Cannot implement 'run' here because it's parameter needs to be inline, and we can't override a non-inline parameter with an inline one
}


sealed trait DatasourceContextInjection
object DatasourceContextInjection {
  sealed trait Implicit extends DatasourceContextInjection
  object Implicit extends Implicit
  sealed trait Member extends DatasourceContextInjection
  object Member extends Member
}

import io.getquill.generic.DecodeAlternate


// TODO Needs to be portable (i.e. plug into current contexts when compiled with Scala 3)
trait Context[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy]
extends ProtoContext[Dialect, Naming]
with EncodingDsl 
with Closeable
{ self =>

  type DatasourceContextBehavior <: DatasourceContextInjection

  //implicit inline def autoDecoder[T]: BaseDecoder[T] = GenericDecoder.generic
  // inline given dec[T](using dec: DecodeAlternate[T, ResultRow]): GenericDecoder[T, ResultRow] with
  //   inline def decode(t: T) = ${ DecodeAlternate[T, ResultRow] }

  inline given dec[T]: GenericDecoder[ResultRow, T] = ${ GenericDecoder[T, ResultRow] }
    

  //def probe(statement: String): Try[_]
  // todo add 'prepare' i.e. encoders here
  //def executeAction[T](cql: String, prepare: Prepare = identityPrepare)(implicit executionContext: ExecutionContext): Result[RunActionResult]

  inline def lift[T](inline runtimeValue: T): T = 
    ${ LiftMacro[T, PrepareRow]('runtimeValue) } // Needs PrepareRow in order to be able to summon encoders

  inline def liftQuery[U[_] <: Iterable[_], T](inline runtimeValue: U[T]): Query[T] = 
    ${ LiftQueryMacro[T, U, PrepareRow]('runtimeValue) }

  extension [T](inline q: Query[T]) {
    inline def filterByKeys(inline map: Map[String, String]) =
      q.filter(p => MapFlicer[T, PrepareRow](p, map, null, (a, b) => (a == b) || (b == (null) ) ))
  }

  protected def context: DatasourceContext = fail(s"DatasourceContext method not implemented for '${this.getClass}' Context")

  import scala.annotation.targetName

  // Think I need to implement 'run' here as opposed to in Context because an abstract
  // inline method cannot be called. Should look into this further. E.g. maybe the 'inline' in
  // the regular context can be non inline
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val ca = new ContextOperation[T, Dialect, Naming, PrepareRow, ResultRow, Result[RunQueryResult[T]]](self.idiom, self.naming) {
      def execute(sql: String, prepare: PrepareRow => (List[Any], PrepareRow), extractorOpt: Option[ResultRow => T], executionType: ExecutionType) =
        val extractor = 
          extractorOpt match
            case Some(e) => e
            case None => throw new IllegalArgumentException("Extractor required")

        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.executeQuery(sql, prepare, extractor)(executionType, runContext)
    }
    // TODO Could make Quoted operation constructor that is a typeclass, not really necessary though
    QueryExecution.apply(QuotedOperation.QueryOp(quoted), ca)
  }

  @targetName("runAction")
  inline def run[T](inline quoted: Quoted[Action[T]]): Result[RunActionResult] = {
    val ca = new ContextOperation[T, Dialect, Naming, PrepareRow, ResultRow, Result[RunActionResult]](self.idiom, self.naming) {
      def execute(sql: String, prepare: PrepareRow => (List[Any], PrepareRow), extractorOpt: Option[ResultRow => T], executionType: ExecutionType) =
        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        self.executeAction(sql, prepare)(executionType, runContext)
    }
    QueryExecution.apply(QuotedOperation.ActionOp(quoted), ca)
  }

  @targetName("runBatchAction")
  inline def run[T, A <: Action[T]](inline quoted: Quoted[BatchAction[A]]): Result[RunBatchActionResult] = {
    val ca = new BatchContextOperation[T, A, Dialect, Naming, PrepareRow, ResultRow, Result[RunBatchActionResult]](self.idiom, self.naming) {
      def execute(sql: String, prepares: List[PrepareRow => (List[Any], PrepareRow)], executionType: ExecutionType) =
        val runContext = DatasourceContextInjectionMacro[DatasourceContextBehavior, DatasourceContext, this.type](context)
        // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
        val group = BatchGroup(sql, prepares)
        self.executeBatchAction(List(group))(executionType, runContext)
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
