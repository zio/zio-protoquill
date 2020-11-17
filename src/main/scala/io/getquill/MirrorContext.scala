package io.getquill

import miniquill.context.mirror._
import io.getquill.context._

class MirrorContext[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy](val idiom: Dialect, val naming: Naming)
extends Context[Dialect, Naming] 
with MirrorDecoders with MirrorEncoders {
  override type Result[T] = T
  override type RunQueryResult[T] = QueryMirror[T]
  override type RunActionResult = ActionMirror

  case class QueryMirror[T](string: String, prepareRow: Prepare, extractor: Extractor[T], executionType: ExecutionType) {
    def string(pretty: Boolean): String =
      if (pretty)
        idiom.format(string)
      else
        string
  }

  case class ActionMirror(string: String, prepareRow: Prepare)

  //prepare: Prepare = identityPrepare, 
  def executeQuery[T](string: String, prepare: Prepare, extractor: Extractor[T] = identityExtractor, executionType: ExecutionType) =
    QueryMirror(string, prepare, extractor, executionType) // TODO Prepare row
  
  def executeAction[T](string: String, prepare: Prepare = identityPrepare) =
    ActionMirror(string, prepare) // TODO Prepare row
}