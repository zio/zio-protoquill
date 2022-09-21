package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ReturnAction}
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
import io.getquill.context.ProtoContextSecundus
import io.getquill.context.AstSplicing
import io.getquill.context.RowContext
import io.getquill.metaprog.etc.ColumnsFlicer
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.OuterSelectWrap
import scala.annotation.tailrec

trait ContextVerbTranslate[+Dialect <: Idiom, +Naming <: NamingStrategy]
    extends ContextTranslateMacro[Dialect, Naming]:
  self: Context[Dialect, Naming] =>
  override type TranslateResult[T] = T
  override def wrap[T](t: => T): T = t
  override def push[A, B](result: A)(f: A => B): B = f(result)
  override def seq[A](list: List[A]): List[A] = list

trait ContextTranslateMacro[+Dialect <: Idiom, +Naming <: NamingStrategy]
    extends ContextTranslateProto[Dialect, Naming]:
  self: Context[Dialect, Naming] =>

  type TranslateResult[T]
  /*
  Need to have a specialized runner for this context otherwise conflicts happen with strange errors happen
  because the result of DatasourceContextInjectionMacro is unaware that the Runner type is unified upstream
  (i.e. in MirrorContext). This might be a Dotty but, not sure. The following error would occur (error description below is simplified):
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
      val groups = arg.groups.map((sql, prepare) => BatchGroup(sql, prepare))
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
      val groups = arg.groups.map((sql, prepare) => BatchGroupReturning(sql, returningExt.returningBehavior, prepare))
      self.translateBatchQueryReturningEndpoint(groups.toList, prettyPrint)(arg.executionInfo, _summonTranslateRunner())
    }
    QueryExecutionBatch.apply(ca, 1)(quoted)
  }
end ContextTranslateMacro

trait ContextTranslateProto[+Dialect <: Idiom, +Naming <: NamingStrategy]:
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

  private[getquill] def prepareParams(statement: String, prepare: Prepare): TranslateResult[Seq[String]]

  @tailrec
  final protected def prepareParam(param: Any): String = param match {
    case None | null => "null"
    case Some(x)     => prepareParam(x)
    case str: String => s"'$str'"
    case _           => param.toString
  }
end ContextTranslateProto
