package io.getquill

import io.getquill.context.mirror._
import io.getquill.context._
import io.getquill.quoter.Quoted
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.generic.GenericDecoder

sealed trait Dummy
object DummyInst extends Dummy

// TODO Dialect <: Idiom, Naming <: NamingStrategy are not actually needed so can remove them but that causes a compile-time error. Need to figure that out
// TODO Modify some major unit tests with embedded etc... to make sure this works in all cases
trait MirrorColumnResolving[Dialect <: Idiom, Naming <: NamingStrategy] { self: MirrorContextBase[Dialect, Naming] =>
  given mirrorResover: ColumnResolver with {
    def apply(resultRow: Row, columnName: String): Int = resultRow.indexOfKey(columnName)
  }
}

class MirrorContext[Dialect <: Idiom, Naming <: NamingStrategy](val idiom: Dialect, val naming: Naming) extends MirrorContextBase[Dialect, Naming]

trait MirrorContextBase[Dialect <: Idiom, Naming <: NamingStrategy]
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

  case class ActionMirror(string: String, prepareRow: PrepareRow, executionType: ExecutionType)
  case class ActionReturningMirror[T](string: String, prepareRow: PrepareRow, extractor: Extractor[T], returningBehavior: ReturnAction)

  def executeQuery[T](string: String, prepare: Prepare, extractor: Extractor[T] = identityExtractor)(executionType: ExecutionType, dc: DatasourceContext) =
    QueryMirror(string, prepare(Row())._2, extractor, executionType)

  def executeAction[T](string: String, prepare: Prepare = identityPrepare)(executionType: ExecutionType, dc: DatasourceContext): Result[RunActionResult] =
    ActionMirror(string, prepare(Row())._2, executionType)
    
  import scala.annotation.targetName

  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = runQueryBase[T](quoted, ())

  @targetName("runAction")
  inline def run[T](inline quoted: Quoted[Action[T]]): Result[RunActionResult] = runActionBase[T](quoted, ())

}