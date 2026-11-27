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

trait ContextVerbPrepareLambda[+Dialect <: Idiom, +Naming <: NamingStrategy] extends ContextVerbPrepare[Dialect, Naming] {
  self: Context[Dialect, Naming] =>

  type PrepareQueryResult = Session => Result[PrepareRow]
  type PrepareActionResult = Session => Result[PrepareRow]
  type PrepareBatchActionResult = Session => Result[List[PrepareRow]]
} // end ContextVerbPrepareLambda
