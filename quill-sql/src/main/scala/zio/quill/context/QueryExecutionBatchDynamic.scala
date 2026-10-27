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
import zio.quill.generic.GenericEncoder
import zio.quill.Planter
import zio.quill.EagerPlanter
import zio.quill.InjectableEagerPlanter
import zio.quill.LazyPlanter
import zio.quill.ast.Ast
import zio.quill.ast.Filter
import zio.quill.ast.Entity
import zio.quill.ast.ScalarTag
import zio.quill.ast.Returning
import zio.quill.ast.ReturningGenerated
import zio.quill.ast
import scala.quoted._
import zio.quill.ast.{Transform, QuotationTag}
import zio.quill.QuotationLot
import zio.quill.metaprog.QuotedExpr
import zio.quill.metaprog.PlanterExpr
import zio.quill.metaprog.EagerEntitiesPlanterExpr
import zio.quill.Planter
import zio.quill.idiom.ReifyStatement
import zio.quill.Query
import zio.quill.Action
import zio.quill.idiom.Idiom
import zio.quill.NamingStrategy
import zio.quill.metaprog.Extractors._
import zio.quill.BatchAction
import zio.quill.metaprog.QuotationLotExpr
import zio.quill.metaprog.QuotationLotExpr._
import zio.quill.util.Format
import zio.quill.context.LiftMacro
import zio.quill.parser.Unlifter
import zio.quill._
import zio.quill.QAC
import zio.quill.parser.Lifter
import zio.quill.metaprog.InjectableEagerPlanterExpr
import _root_.zio.quill.norm.BetaReduction
import zio.quill.context.Execution.ElaborationBehavior
import zio.quill.quat.Quat
import zio.quill.quat.QuatMaking
import zio.quill.metaprog.EagerListPlanterExpr
import zio.quill.metaprog.EagerPlanterExpr
import zio.quill.metaprog.SummonTranspileConfig
import zio.quill.norm.TranspileConfig
import zio.quill.metaprog.TranspileConfigLiftable
import zio.quill.idiom.Token
import zio.quill.ast.External.Source
import zio.quill.ast.Ident
import zio.quill.util.TraceConfig

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

    // println(s"===== Spliced Ast: ====\n${zio.quill.util.Messages.qprint(ast)}")
    // println(s"===== Initial Lifts: ====\n${zio.quill.util.Messages.qprint(lifts)}")

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

    val executionAst = if (spliceAst) outputAst else zio.quill.ast.NullValue
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
          (ScalarTag(uuid, Source.Parser), InjectableEagerPlanter((t: Any) => t, planter.encoder.asInstanceOf[zio.quill.generic.GenericEncoder[Any, PrepareRow, Session]], uuid))
        // create the full batch-query Ast using the value of actual query of the batch statement i.e. I in:
        // liftQuery[...](...).foreach(p => query[I].insertValue(p))
        val (foreachIdent, actionQueryAst, batchActionType) = PrepareBatchComponents[I, PrepareRow](ast, foreachReplacementAst, extractionBehavior, traceConfig).rightOrException()
        // return the combined batch components
        (foreachIdent, actionQueryAst, batchActionType, List(perRowLift))
    }

} // end QueryExecutionBatchDynamic
