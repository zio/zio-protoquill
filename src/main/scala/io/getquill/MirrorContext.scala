package io.getquill

import miniquill.context.mirror._
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.context._

class MirrorContext[Dialect <: Idiom, Naming <: NamingStrategy](val idiom: Idiom, val naming: Naming)
extends Context[Dialect, Naming] 
with MirrorDecoders {
  override type Result[T] = T
  override type RunQueryResult[T] = QueryMirror[T]

  case class QueryMirror[T](string: String, prepareRow: PrepareRow, extractor: Extractor[T]) {
    def string(pretty: Boolean): String =
      if (pretty)
        idiom.format(string)
      else
        string
  }

  //prepare: Prepare = identityPrepare, 
  def executeQuery[T](string: String, extractor: Extractor[T] = identityExtractor) =
    QueryMirror(string, null, extractor)
    //QueryMirror(string, prepare(Row())._2, extractor)
}