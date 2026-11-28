// Standin for ProtoContextSecundus that we get from Scala2-Quill that needs to be updated
// with executeActionReturningMany
package zio.quill.context

import zio.quill.{NamingStrategy, ReturnAction}

/**
 * A common context used between Quill and ProtoQuill. This is more like a pre-context because the actual `run`
 * methods cannot be contained here since they use macros. Right now not all Scala2-Quill context extend
 * this context but hopefully they will all in the future. This will establish a common general-api that
 * Quill contexts can use.
 * In ProtoQuill, this context is used for the base of all other context and allows the Scala 3 macros
 * to call the `execute___` methods. In Scala2-Quill wherein macros are less strict about signatures,
 * this cannot be used for `Context` (in `Context.scala`) but various higher-level context extend it
 * as a guard-rail against API drift i.e. so that the Scala2-Quill and ProtoQuill internal-context
 * APIs remain largely the same.
 */
trait ProtoContextSecundus[+Dialect <: zio.quill.idiom.Idiom, +Naming <: NamingStrategy] extends RowContext {
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
  type Runner

  def idiom: Dialect
  def naming: Naming

  def executeQuery[T](sql: String, prepare: Prepare, extractor: Extractor[T])(executionInfo: ExecutionInfo, rn: Runner): Result[RunQueryResult[T]]
  def executeQuerySingle[T](string: String, prepare: Prepare, extractor: Extractor[T])(executionInfo: ExecutionInfo, rn: Runner): Result[RunQuerySingleResult[T]]
  def executeAction(sql: String, prepare: Prepare)(executionInfo: ExecutionInfo, rn: Runner): Result[RunActionResult]
  def executeActionReturning[T](sql: String, prepare: Prepare, extractor: Extractor[T], returningBehavior: ReturnAction)(executionInfo: ExecutionInfo, rn: Runner): Result[RunActionReturningResult[T]]
  def executeActionReturningMany[T](sql: String, prepare: Prepare, extractor: Extractor[T], returningBehavior: ReturnAction)(executionInfo: ExecutionInfo, rn: Runner): Result[RunActionReturningResult[List[T]]]
  def executeBatchAction(groups: List[BatchGroup])(executionInfo: ExecutionInfo, rn: Runner): Result[RunBatchActionResult]
  def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(executionInfo: ExecutionInfo, rn: Runner): Result[RunBatchActionReturningResult[T]]
}
