package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.ReturnAction
import io.getquill.generic.StandardCodec
import io.getquill.Quoted
import io.getquill.QueryMeta
import io.getquill.generic._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericDecoder
import io.getquill.generic.GenericEncoder
import io.getquill.Planter
import io.getquill.EagerPlanter
import io.getquill.InjectableEagerPlanter
import io.getquill.LazyPlanter
import io.getquill.ast.Ast
import io.getquill.ast.Filter
import io.getquill.ast.Entity
import io.getquill.ast.ScalarTag
import io.getquill.ast.Returning
import io.getquill.ast.ReturningGenerated
import io.getquill.ast
import scala.quoted._
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.metaprog.EagerEntitiesPlanterExpr
import io.getquill.Planter
import io.getquill.idiom.ReifyStatement
import io.getquill.Query
import io.getquill.Action
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.metaprog.Extractors._
import io.getquill.BatchAction
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._
import io.getquill.util.Format
import io.getquill.context.LiftMacro
import io.getquill.parser.Unlifter
import io.getquill._
import io.getquill.QAC
import io.getquill.parser.Lifter
import io.getquill.metaprog.InjectableEagerPlanterExpr
import _root_.io.getquill.norm.BetaReduction
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.quat.Quat
import io.getquill.quat.QuatMaking
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.SummonTranspileConfig
import io.getquill.norm.TranspileConfig
import io.getquill.metaprog.TranspileConfigLiftable
import io.getquill.idiom.Token
import io.getquill.ast.External.Source
import io.getquill.ast.Ident
import io.getquill.util.TraceConfig

object QueryExecutionBatchDynamic {
  import QueryExecutionBatchModel._
  import PrepareDynamicExecution._

  def apply[
      I,
      T,
      A <: QAC[I, T] & Action[I],
      ResultRow,
      PrepareRow,
      Session,
      D <: Idiom,
      N <: NamingStrategy,
      Ctx <: Context[_, _],
      Res
  ](
      quotedRaw: Quoted[BatchAction[A]],
      batchContextOperation: ContextOperation.Batch[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res],
      extractionBehavior: BatchExtractBehavior,
      rawExtractor: Extraction[ResultRow, Session, T],
      topLevelQuat: Quat,
      transpileConfig: TranspileConfig,
      batchingBehavior: BatchingBehavior,
      spliceAst: Boolean
  ) = {
    val quotedDeduped = quotedRaw.dedupeRuntimeBinds

    // since real quotation could possibly be nested, need to get all splice all quotes and get all lifts in all runtimeQuote sections first
    val ast = spliceQuotations(quotedDeduped)
    val lifts = gatherLifts(quotedDeduped)
    val idiom = batchContextOperation.idiom
    val naming = batchContextOperation.naming

    // println(s"===== Spliced Ast: ====\n${io.getquill.util.Messages.qprint(ast)}")
    // println(s"===== Initial Lifts: ====\n${io.getquill.util.Messages.qprint(lifts)}")

    // Given: Person(name, age)
    // For the query:
    //   liftQuery(List(Person("Joe", 123))).foreach(p => query[Person].insertValue(p))
    //   it would be (CaseClass(name->lift(A), age->lift(B)), BatchActionType.Insert, List(InjectableEagerLift(A), InjectableEagerLift(B))))
    // Same thing regardless of what kind of object is in the insert:
    //   liftQuery(List("foo")).foreach(name => query[Person].update(_.name -> name))
    //   it would be (CaseClass(name->lift(A), age->lift(B)), BatchActionType.Update, List(InjectableEagerLift(A), InjectableEagerLift(B))))
    //
    // That is why it is important to find the actual EagerEntitiesPlanterExpr (i.e. the part defined by `query[Person]`). That
    // way we know the actual entity that needs to be lifted.
    val (primaryPlanter, categorizedPlanters) = organizePlanters(lifts)

    // Use some custom functionality in the lift macro to prepare the case class an injectable lifts
    // e.g. if T is Person(name: String, age: Int) and we do liftQuery(people:List[Person]).foreach(p => query[Person].insertValue(p))
    // Then:
    //   ast = CaseClass(name -> lift(UUID1), age -> lift(UUID2))  // NOTE: lift in the AST means a ScalarTag
    //   lifts = List(InjectableEagerLift(p.name, UUID1), InjectableEagerLift(p.age, UUID2))
    // e.g. if T is String and we do liftQuery(people:List[String]).foreach(p => query[Person].insertValue(Person(p, 123)))
    // Then:
    //   ast = lift(UUID1)  // I.e. ScalarTag(UUID1) since lift in the AST means a ScalarTag
    //   lifts = List(InjectableEagerLift(p, UUID1))
    val (foreachIdent, actionQueryAst, batchActionType, perRowLifts) =
      extractPrimaryComponents[I, PrepareRow, Session](primaryPlanter, ast, extractionBehavior, transpileConfig.traceConfig)

    // equivalent to static expandQuotation result
    val dynamicExpandedQuotation =
      batchActionType match {
        case BatchActionType.Insert => Quoted[Insert[I]](actionQueryAst, perRowLifts, Nil) // Already gathered queries and lifts from sub-clauses, don't need them anymore
        case BatchActionType.Update => Quoted[Update[I]](actionQueryAst, perRowLifts, Nil)
        // We need lifts for 'Delete' because it could have a WHERE clause
        case BatchActionType.Delete => Quoted[Delete[I]](actionQueryAst, perRowLifts, Nil)
      }

    val (stmt, outputAst, sortedLiftsRaw, extractor, sortedSecondaryLifts) =
      PrepareDynamicExecution[I, T, T, D, N, PrepareRow, ResultRow, Session](
        dynamicExpandedQuotation,
        rawExtractor,
        idiom,
        naming,
        ElaborationBehavior.Skip,
        topLevelQuat,
        transpileConfig,
        SpliceBehavior.AlreadySpliced,
        categorizedPlanters.map(_.planter),
        Some(foreachIdent.name)
      )

    val sortedLifts = sortedLiftsRaw.asInstanceOf[List[InjectableEagerPlanter[_, _, _]]]

    def expandLiftQueryMembers(filteredPerRowLifts: List[Planter[?, ?, ?]], entities: Iterable[?]) =
      entities.map { entity =>
        SingleEntityLifts(sortedLifts.map(lift => lift.withInject(entity)))
      }

    // Get the planters needed for every element lift (see primaryPlanterLifts in BatchStatic for more detail)
    val primaryPlanterLifts =
      primaryPlanter match {
        case PlanterKind.PrimaryEntitiesList(entitiesPlanter) =>
          expandLiftQueryMembers(sortedLifts, entitiesPlanter.value).toList
        case PlanterKind.PrimaryScalarList(scalarsPlanter) =>
          expandLiftQueryMembers(sortedLifts, scalarsPlanter.values).toList
      }

    val (unparticularQuery, _) = Unparticular.Query.fromStatement(stmt, batchContextOperation.idiom.liftingPlaceholder)

    val batchGroups =
      QueryExecutionBatchIteration[PrepareRow, Session](
        batchContextOperation.idiom,
        unparticularQuery,
        primaryPlanterLifts,
        sortedSecondaryLifts,
        sortedLifts,
        idiom.liftingPlaceholder,
        idiom.emptySetContainsToken,
        batchingBehavior,
        extractionBehavior
      )(transpileConfig.traceConfig)

    val executionAst = if (spliceAst) outputAst else io.getquill.ast.NullValue
    batchContextOperation.execute(ContextOperation.BatchArgument(batchGroups, extractor, ExecutionInfo(ExecutionType.Dynamic, executionAst, topLevelQuat), None))
  }

  extension [T](element: Either[String, T]) {
    def rightOrException() =
      element match {
        case Right(value) => value
        case Left(error)  => throw new IllegalArgumentException(error)
      }
  }

  // NOTE We don't need to process secondary planters anymore because that list is not being used.
  // It is handled by the static state. Can removing everything having to do with secondary planters list in a future PR.
  sealed trait PlanterKind
  object PlanterKind {
    case class PrimaryEntitiesList(planter: EagerEntitiesPlanter[?, ?, ?]) extends PlanterKind
    case class PrimaryScalarList(planter: EagerListPlanter[?, ?, ?]) extends PlanterKind
    case class Other(planter: Planter[?, ?, ?]) extends PlanterKind
  }

  def organizePlanters(planters: List[Planter[?, ?, ?]]) =
    planters.foldLeft((Option.empty[PlanterKind.PrimaryEntitiesList | PlanterKind.PrimaryScalarList], List.empty[PlanterKind.Other])) {
      case ((None, list), planter: EagerEntitiesPlanter[?, ?, ?]) =>
        val planterKind = PlanterKind.PrimaryEntitiesList(planter)
        (Some(planterKind), list)
      case ((None, list), planter: EagerListPlanter[?, ?, ?]) =>
        val planterKind = PlanterKind.PrimaryScalarList(planter)
        (Some(planterKind), list)
      case ((primary @ Some(_), list), planter) =>
        (primary, list :+ PlanterKind.Other(planter))
      // this means we haven't found the primary planter yet (don't think this can happen because nothing can be before liftQuery), keep going
      case ((primary @ None, list), planter) =>
        throw new IllegalArgumentException("Invalid planter traversal")
    } match {
      case (Some(primary), categorizedPlanters) => (primary, categorizedPlanters)
      case (None, _)                            => throw new IllegalArgumentException(s"Could not find an entities list-lift (i.e. liftQuery(entities/scalars) in liftQuery(...).foreach()) in lifts: ${planters}")
    }

  def extractPrimaryComponents[I, PrepareRow, Session](
      primaryPlanter: PlanterKind.PrimaryEntitiesList | PlanterKind.PrimaryScalarList,
      ast: Ast,
      extractionBehavior: QueryExecutionBatchModel.BatchExtractBehavior,
      traceConfig: TraceConfig
  ): (Ident, Ast, BatchActionType, List[InjectableEagerPlanter[?, PrepareRow, Session]]) =
    primaryPlanter match {
      // In the case of liftQuery(entities)
      case PlanterKind.PrimaryEntitiesList(planter) =>
        val (foreachIdent, actionQueryAst, batchActionType) = PrepareBatchComponents[I, PrepareRow](ast, planter.fieldClass, extractionBehavior, traceConfig).rightOrException()
        (foreachIdent, actionQueryAst, batchActionType, planter.fieldGetters.asInstanceOf[List[InjectableEagerPlanter[?, PrepareRow, Session]]])
      // In the case of liftQuery(scalars)
      // Note, we could have potential other liftQuery(scalars) later in the query for example:
      // liftQuery(List("Joe","Jack","Jill")).foreach(query[Person].filter(name => liftQuery(1,2,3 /*ids of Joe,Jack,Jill respectively*/).contains(p.id)).update(_.name -> name))
      // Therefore we cannot assume that there is only one
      case PlanterKind.PrimaryScalarList(planter) =>
        val uuid = java.util.UUID.randomUUID.toString
        val (foreachReplacementAst, perRowLift) =
          (ScalarTag(uuid, Source.Parser), InjectableEagerPlanter((t: Any) => t, planter.encoder.asInstanceOf[io.getquill.generic.GenericEncoder[Any, PrepareRow, Session]], uuid))
        // create the full batch-query Ast using the value of actual query of the batch statement i.e. I in:
        // liftQuery[...](...).foreach(p => query[I].insertValue(p))
        val (foreachIdent, actionQueryAst, batchActionType) = PrepareBatchComponents[I, PrepareRow](ast, foreachReplacementAst, extractionBehavior, traceConfig).rightOrException()
        // return the combined batch components
        (foreachIdent, actionQueryAst, batchActionType, List(perRowLift))
    }

} // end QueryExecutionBatchDynamic
