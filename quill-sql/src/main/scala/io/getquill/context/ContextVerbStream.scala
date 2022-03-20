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

trait ContextVerbStream[Dialect <: io.getquill.idiom.Idiom, Naming <: NamingStrategy] extends ProtoStreamContext[Dialect, Naming]:
  self: Context[Dialect, Naming] =>

  // Must be lazy since idiom/naming are null (in some contexts) initially due to initialization order
  private lazy val make = ContextOperation.Factory[Dialect, Naming, PrepareRow, ResultRow, Session, this.type](self.idiom, self.naming)

  @targetName("streamQuery")
  inline def stream[T](inline quoted: Quoted[Query[T]]): StreamResult[T] = _streamInternal[T](quoted, None)
  @targetName("streamQueryWithFetchSize")
  inline def stream[T](inline quoted: Quoted[Query[T]], fetchSize: Int): StreamResult[T] = _streamInternal[T](quoted, Some(fetchSize))

  /** Internal API that cannot be made private due to how inline functions */
  inline def _streamInternal[T](inline quoted: Quoted[Query[T]], fetchSize: Option[Int]): StreamResult[T] = {
    val ca = make.op[Nothing, T, StreamResult[T]] { arg =>
      val simpleExt = arg.extractor.requireSimple()
      self.streamQuery(arg.fetchSize, arg.sql, arg.prepare.head, simpleExt.extract)(arg.executionInfo, InternalApi._summonRunner())
    }
    QueryExecution.apply(quoted, ca, fetchSize)
  }
end ContextVerbStream