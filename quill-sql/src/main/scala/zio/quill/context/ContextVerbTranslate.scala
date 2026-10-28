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
import scala.annotation.tailrec

trait ContextVerbTranslate[+Dialect <: Idiom, +Naming <: NamingStrategy]
    extends ContextTranslateMacro[Dialect, Naming] {
  self: Context[Dialect, Naming] =>
  override type TranslateResult[T] = T
  override def wrap[T](t: => T): T = t
  override def push[A, B](result: A)(f: A => B): B = f(result)
  override def seq[A](list: List[A]): List[A] = list
}

trait ContextTranslateMacro[+Dialect <: Idiom, +Naming <: NamingStrategy]
    extends ContextTranslateProto[Dialect, Naming] {
  self: Context[Dialect, Naming] =>

  type TranslateResult[T]
  /*
  Need to have a specialized runner for this context otherwise conflicts happen with strange errors happen
  because the result of DatasourceContextInjectionMacro is unaware that the Runner type is unified upstream
  (i.e. in MirrorContext). This might be a Dotty but, not sure. The folloiwng error would occur (error description below is simplified):
  Name clash between inherited members:
  def inline$context$i1(x$0: Context[Dialect, Naming] & ContextVerbPrepare[Dialect, Naming]): ContextVerbPrepare.this.Runner in trait ContextVerbPrepare and
  def inline$context$i1(x$0: Context[Dialect, Naming] & ContextTranslateMacro[Dialect, Naming]): ContextTranslateMacro.this.Runner in trait ContextTranslateMacro
  have the same type after erasure.
  (Note that RunnerBehavior however can be the same)
   */
  type TranslateRunner
  type RunnerBehavior <: RunnerSummoningBehavior
  def translateContext: TranslateRunner

  /** Internal API that cannot be made private due to how inline functions */
  inline def _summonTranslateRunner() = DatasourceContextInjectionMacro[RunnerBehavior, TranslateRunner, this.type](translateContext)

  // Must be lazy since idiom/naming are null (in some contexts) initially due to initialization order
  private lazy val make = ContextOperation.Factory[Dialect, Naming, PrepareRow, ResultRow, Session, this.type](self.idiom, self.naming)

  @targetName("translateQuery")
  inline def translate[T](inline quoted: Quoted[Query[T]]): TranslateResult[String] = translate(quoted, false)
  @targetName("translateQuery")
  inline def translate[T](inline quoted: Quoted[Query[T]], inline prettyPrint: Boolean): TranslateResult[String] = {
    val ca = make.op[Nothing, T, TranslateResult[String]] { arg =>
      val simpleExt = arg.extractor.requireSimple()
      self.translateQueryEndpoint(arg.sql, arg.prepare, simpleExt.extract, prettyPrint)(arg.executionInfo, _summonTranslateRunner())
    }
    QueryExecution.apply(ca)(quoted, None)
  }

  @targetName("translateQuerySingle")
  inline def translate[T](inline quoted: Quoted[T]): TranslateResult[String] = translate(quoted, false)
  @targetName("translateQuerySingle")
  inline def translate[T](inline quoted: Quoted[T], inline prettyPrint: Boolean): TranslateResult[String] = {
    val ca = make.op[Nothing, T, TranslateResult[String]] { arg =>
      val simpleExt = arg.extractor.requireSimple()
      self.translateQueryEndpoint(arg.sql, arg.prepare, simpleExt.extract, prettyPrint)(arg.executionInfo, _summonTranslateRunner())
    }
    QueryExecution.apply(ca)(QuerySingleAsQuery(quoted), None)
  }

  @targetName("translateAction")
  inline def translate[E](inline quoted: Quoted[Action[E]]): TranslateResult[String] = translate(quoted, false)
  @targetName("translateAction")
  inline def translate[E](inline quoted: Quoted[Action[E]], inline prettyPrint: Boolean): TranslateResult[String] = {
    val ca = make.op[E, Any, TranslateResult[String]] { arg =>
      self.translateQueryEndpoint(arg.sql, arg.prepare, prettyPrint = prettyPrint)(arg.executionInfo, _summonTranslateRunner())
    }
    QueryExecution.apply(ca)(quoted, None)
  }

  @targetName("translateActionReturning")
  inline def translate[E, T](inline quoted: Quoted[ActionReturning[E, T]]): TranslateResult[String] = translate(quoted, false)
  @targetName("translateActionReturning")
  inline def translate[E, T](inline quoted: Quoted[ActionReturning[E, T]], inline prettyPrint: Boolean): TranslateResult[String] = {
    val ca = make.op[E, T, TranslateResult[String]] { arg =>
      val returningExt = arg.extractor.requireReturning()
      self.translateQueryEndpoint(arg.sql, arg.prepare, returningExt.extract, prettyPrint)(arg.executionInfo, _summonTranslateRunner())
    }
    QueryExecution.apply(ca)(quoted, None)
  }

  @targetName("translateBatchAction")
  inline def translate[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): TranslateResult[List[String]] = translate(quoted, false)
  @targetName("translateBatchAction")
  inline def translate[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]], inline prettyPrint: Boolean): TranslateResult[List[String]] = {
    val ca = make.batch[I, Nothing, A, TranslateResult[List[String]]] { arg =>
      // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
      val groups = arg.groups.map((sql, prepare) => BatchGroup(sql, prepare, Nil))
      self.translateBatchQueryEndpoint(groups.toList, prettyPrint)(arg.executionInfo, _summonTranslateRunner())
    }
    QueryExecutionBatch.apply(ca, 1)(quoted)
  }

  @targetName("translateBatchActionReturning")
  inline def translate[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): TranslateResult[List[String]] = translate(quoted, false)
  @targetName("translateBatchActionReturning")
  inline def translate[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]], inline prettyPrint: Boolean): TranslateResult[List[String]] = {
    val ca = make.batch[I, T, A, TranslateResult[List[String]]] { arg =>
      val returningExt = arg.extractor.requireReturning()
      // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
      val groups = arg.groups.map((sql, prepare) => BatchGroupReturning(sql, returningExt.returningBehavior, prepare, Nil))
      self.translateBatchQueryReturningEndpoint(groups.toList, prettyPrint)(arg.executionInfo, _summonTranslateRunner())
    }
    QueryExecutionBatch.apply(ca, 1)(quoted)
  }
} // end ContextTranslateMacro

trait ContextTranslateProto[+Dialect <: Idiom, +Naming <: NamingStrategy] {
  self: Context[Dialect, Naming] =>

  type TranslateResult[T]
  type TranslateRunner

  def wrap[T](t: => T): TranslateResult[T]
  def push[A, B](result: TranslateResult[A])(f: A => B): TranslateResult[B]
  def seq[A](list: List[TranslateResult[A]]): TranslateResult[List[A]]

  def translateQueryEndpoint[T](statement: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor, prettyPrint: Boolean = false)(executionInfo: ExecutionInfo, dc: TranslateRunner): TranslateResult[String] =
    push(prepareParams(statement, prepare)) { params =>
      val query =
        if (params.nonEmpty) {
          params.foldLeft(statement) {
            case (expanded, param) => expanded.replaceFirst("\\?", param)
          }
        } else {
          statement
        }

      if (prettyPrint)
        idiom.format(query)
      else
        query
    }

  def translateBatchQueryEndpoint(groups: List[BatchGroup], prettyPrint: Boolean = false)(executionInfo: ExecutionInfo, dc: TranslateRunner): TranslateResult[List[String]] =
    seq {
      groups.flatMap { group =>
        group.prepare.map { prepare =>
          translateQueryEndpoint(group.string, prepare, prettyPrint = prettyPrint)(executionInfo, dc)
        }
      }
    }

  def translateBatchQueryReturningEndpoint(groups: List[BatchGroupReturning], prettyPrint: Boolean = false)(executionInfo: ExecutionInfo, dc: TranslateRunner): TranslateResult[List[String]] =
    seq {
      groups.flatMap { group =>
        group.prepare.map { prepare =>
          translateQueryEndpoint(group.string, prepare, prettyPrint = prettyPrint)(executionInfo, dc)
        }
      }
    }

  private[quill] def prepareParams(statement: String, prepare: Prepare): TranslateResult[Seq[String]]

  @tailrec
  final protected def prepareParam(param: Any): String = param match {
    case None | null => "null"
    case Some(x)     => prepareParam(x)
    case str: String => s"'$str'"
    case _           => param.toString
  }
} // end ContextTranslateProto
