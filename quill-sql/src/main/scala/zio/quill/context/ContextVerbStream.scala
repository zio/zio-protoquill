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

trait ContextVerbStream[+Dialect <: zio.quill.idiom.Idiom, +Naming <: NamingStrategy] extends ProtoStreamContext[Dialect, Naming] {
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
      self.streamQuery(arg.fetchSize, arg.sql, arg.prepare, simpleExt.extract)(arg.executionInfo, InternalApi._summonRunner())
    }
    QueryExecution.apply(ca)(quoted, fetchSize)
  }
} // end ContextVerbStream
