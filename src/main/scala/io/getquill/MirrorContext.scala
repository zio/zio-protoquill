package io.getquill

import miniquill.context.mirror._
import io.getquill.context._
import miniquill.quoter.Quoted

sealed trait Dummy
object DummyInst extends Dummy

class MirrorContext[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy](val idiom: Dialect, val naming: Naming)
extends Context[Dialect, Naming] 
with MirrorDecoders with MirrorEncoders { self =>
  override type Result[T] = T
  override type RunQueryResult[T] = QueryMirror[T]
  override type PrepareRow = Row
  override type ResultRow = Row
  override type RunActionResult = ActionMirror
  override type RunActionReturningResult[T] = ActionReturningMirror[T]

  override type DatasourceContext = Unit

  implicit val d: Dummy = DummyInst

  case class QueryMirror[T](string: String, prepareRow: PrepareRow, extractor: Extractor[T], executionType: ExecutionType) {
    def string(pretty: Boolean): String =
      if (pretty)
        idiom.format(string)
      else
        string
  }

  case class ActionMirror(string: String, prepareRow: PrepareRow)
  case class ActionReturningMirror[T](string: String, prepareRow: PrepareRow, extractor: Extractor[T], returningBehavior: ReturnAction)

  def executeQuery[T](string: String, prepare: Prepare, extractor: Extractor[T] = identityExtractor)(executionType: ExecutionType, dc: DatasourceContext) =
    QueryMirror(string, prepare(Row())._2, extractor, executionType)

  def executeAction[T](string: String, prepare: Prepare = identityPrepare)(executionType: ExecutionType, dc: DatasourceContext): Result[RunActionResult] =
    ActionMirror(string, prepare(Row())._2)
    
  import scala.annotation.targetName

  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = runQueryBase[T](quoted, ())

  @targetName("runAction")
  inline def run[T](inline quoted: Quoted[Action[T]]): Result[RunActionResult] = runActionBase[T](quoted, ())

}