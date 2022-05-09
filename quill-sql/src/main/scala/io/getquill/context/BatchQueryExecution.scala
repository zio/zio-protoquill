package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.generic.Dsl
//import io.getquill.util.Messages.fail
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

// trait BatchContextOperation[I, T, A <: QAC[I, T] with Action[I], D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Session, Res](val idiom: D, val naming: N) {
//   def execute(sql: String, prepare: List[(PrepareRow, Session) => (List[Any], PrepareRow)], extractor: Extraction[ResultRow, Session, T], executionInfo: ExecutionInfo): Res
// }

case class Shard[T](members: List[T]):
  def add(t: T) = Shard(members :+ t)

private[getquill] enum BatchActionType:
  case Insert
  case Update
  case Delete

private[getquill] object ActionEntity:
  def unapply(actionAst: Ast): Option[BatchActionType] =
    actionAst match
      case ast.Insert(entity, _)           => Some(BatchActionType.Insert)
      case ast.Update(entity, assignments) => Some(BatchActionType.Update)
      case ast.Delete(entity)              => Some(BatchActionType.Delete)
      case _                               => None

object PrepareBatchComponents:
  import Execution._
  import BatchQueryExecutionModel._

  def apply[I, PrepareRow](unliftedAst: Ast, foreachIdentAst: ast.Ast, extractionBehavior: BatchExtractBehavior): Either[String, (Ast, BatchActionType)] = {
    // putting this in a block since I don't want to externally import these packages
    import io.getquill.ast._
    val componentsOrError =
      extractionBehavior match
        case ExtractBehavior.Skip =>
          unliftedAst match
            // TODO In the actionQueryAst should we make sure to verify that an Entity exists?
            case Foreach(_, foreachIdent, actionQueryAst @ ActionEntity(bType)) =>
              Right(foreachIdent, actionQueryAst, bType)
            case other =>
              Left(s"Malformed batch entity: ${io.getquill.util.Messages.qprint(other)}. Batch insertion entities must have the form Insert(Entity, Nil: List[Assignment])")

        case ExtractBehavior.ExtractWithReturnAction =>
          unliftedAst match
            // Designed to Match:          liftQuery(...).foreach(p => query[Person].insert(...))
            // Also Matches:               liftQuery(...).foreach(p => query[Person].filter(...).insert(...))
            // but more generally matches: liftQuery(...).foreach(p => {stuff})
            // TODO In the actionQueryAst should we make sure to verify that an Entity exists?
            case Foreach(_, foreachIdent, actionQueryAst @ ReturningAction(ActionEntity(bType), id, body)) =>
              actionQueryAst match
                case _: Returning          => Right(foreachIdent, actionQueryAst, bType)
                case _: ReturningGenerated => Right(foreachIdent, actionQueryAst, bType)
            case other =>
              Left(s"Malformed batch entity: ${other}. Batch insertion entities must have the form Returning/ReturningGenerated(Insert(Entity, Nil: List[Assignment]), _, _)")

    // TODO NOT NECESSARILY A CASE CLASS ANOT NOT NECESSARILY THE TYPE BEING INSERTED. NEED TO TEST WITH DIFF
    // TYPES BEING ENCODED AND INSERTED TOO
    // e.g. liftQuery(Vip, Vip).foreach(v => query[Person].insertValue(Person(v.name, v.age)))
    // OR WITH SCALARS
    // e.g. liftQuery(1, 2).foreach(i => query[Person].insertValue(Person("Joe", i)))

    // (continue to beta-reduce out the foreach-ident if an error has not happened)
    componentsOrError.map { (foreachIdent, actionQueryAstRaw, bType) =>
      // The primary idea that drives batch query execution is the realization that you
      // can beta reduce out the foreach identifier replacing it with lift tags.
      // For example if we have something like:
      // actionQueryAstRaw: liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).update(_.name == p.name))
      // where Person(id: Int, name: String, age: Int)
      // all we need do do is to take the caseClassAst which is ast.CaseClass(p.id -> ScalarTag(A), p.name -> ScalarTag(B), p.age -> ScalarTag(C))
      // (and corresponding perRowLifts (EagerPlanter(A), EagerPlanter(B), EagerPlanter(C)))
      // then do a beta reduction which will turn our actionQueryAstRaw into:
      // actionQueryAstRaw: liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == ScalarTag(A)).update(_.name == ScalarTag(B)))
      // this will ultimately yield a query that looks like: UPDATE Person SET name = ? WHERE id = ? and for each person entity
      // the corresponding values will be plugged in
      val actionQueryAst = BetaReduction(actionQueryAstRaw, foreachIdent -> foreachIdentAst)
      // println(s"==== Reduced AST: ${io.getquill.util.Messages.qprint(actionQueryAst)}")
      (actionQueryAst, bType)
    }
  }
end PrepareBatchComponents

object BatchQueryExecutionModel:
  import Execution._
  type BatchExtractBehavior = ExtractBehavior.Skip.type | ExtractBehavior.ExtractWithReturnAction.type
  given ToExpr[BatchExtractBehavior] with
    def apply(behavior: BatchExtractBehavior)(using Quotes): Expr[BatchExtractBehavior] =
      behavior match
        case _: ExtractBehavior.Skip.type                    => '{ ExtractBehavior.Skip }
        case _: ExtractBehavior.ExtractWithReturnAction.type => '{ ExtractBehavior.ExtractWithReturnAction }

object DynamicBatchQueryExecution:
  import BatchQueryExecutionModel._
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
      batchContextOperation: ContextOperation[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res],
      caseClassAst: ast.CaseClass,
      // These are computed based on the insertion-type I which is calculated before, not from quotedRaw
      // (i.e. note that to get lifts from QuotedRaw we need to go through runtimeQuotes of each quote recursively so it wouldn't be possible to know that anyway for a dynamic query during compile-time)
      perRowLifts: List[Planter[_, _, _]],
      extractionBehavior: BatchExtractBehavior,
      rawExtractor: Extraction[ResultRow, Session, T],
      topLevelQuat: Quat
  ) = {
    // since real quotation could possibly be nested, need to get all splice all quotes and get all lifts in all runtimeQuote sections first
    val ast = spliceQuotations(quotedRaw)
    val lifts = gatherLifts(quotedRaw)

    // println(s"===== Spliced Ast: ====\n${io.getquill.util.Messages.qprint(ast)}")
    // println(s"===== Initial Lifts: ====\n${io.getquill.util.Messages.qprint(lifts)}")

    val entities =
      lifts match
        case EagerEntitiesPlanter(value, _) :: Nil => value
        case _                                     => throw new IllegalStateException(s"Invalid lifts instance: ${quotedRaw.lifts}. Must be a single InjectableEagerPlanter instance")

    val (actionQueryAst, batchActionType) =
      PrepareBatchComponents[I, PrepareRow](ast, caseClassAst, extractionBehavior) match
        case Right(value) => value
        case Left(error) =>
          throw new IllegalStateException(error)

    // println(s"===== ActionQueryAst: ====\n${io.getquill.util.Messages.qprint(actionQueryAst)}")
    // println(s"===== Per-Row Lifts: ====\n${io.getquill.util.Messages.qprint(perRowLifts)}")

    // equivalent to static expandQuotation result
    val dynamicExpandedQuotation =
      batchActionType match
        case BatchActionType.Insert => Quoted[Insert[I]](actionQueryAst, perRowLifts, Nil) // Already gathered queries and lifts from sub-clauses, don't need them anymore
        case BatchActionType.Update => Quoted[Update[I]](actionQueryAst, perRowLifts, Nil)
        // We need lifts for 'Delete' because it could have a WHERE clause
        case BatchActionType.Delete => Quoted[Delete[I]](actionQueryAst, perRowLifts, Nil)

    // println(s"===== Dynamically Expanded Quotation: ====\n${io.getquill.util.Messages.qprint(dynamicExpandedQuotation)}")

    val (queryString, outputAst, sortedLifts, extractor) =
      PrepareDynamicExecution[I, T, T, D, N, PrepareRow, ResultRow, Session](
        dynamicExpandedQuotation,
        rawExtractor,
        batchContextOperation.idiom,
        batchContextOperation.naming,
        ElaborationBehavior.Skip,
        topLevelQuat,
        SpliceBehavior.AlreadySpliced
      )

    // println(s"===== Entities: ====\n${io.getquill.util.Messages.qprint(entities.toList)}")

    val prepares =
      entities.map { entity =>
        val lifts = sortedLifts.asInstanceOf[List[InjectableEagerPlanter[_, _, _]]].map(lift => lift.withInject(entity))
        // println(s"===== Prepared Lifts: ====\n${io.getquill.util.Messages.qprint(lifts)}")
        (row: PrepareRow, session: Session) => LiftsExtractor.Dynamic[PrepareRow, Session](lifts, row, session)
      }

    // TODO this variable should go through BatchQueryExecution arguments and be propagated here
    val spliceAst = false
    val executionAst = if (spliceAst) outputAst else io.getquill.ast.NullValue
    batchContextOperation.execute(ContextOperation.Argument(queryString, prepares.toArray, extractor, ExecutionInfo(ExecutionType.Dynamic, executionAst, topLevelQuat), None))
  }

object BatchQueryExecution:
  import Execution._
  import BatchQueryExecutionModel.{_, given}

  private[getquill] class RunQuery[
      I: Type,
      T: Type,
      A <: QAC[I, T] & Action[I]: Type,
      ResultRow: Type,
      PrepareRow: Type,
      Session: Type,
      D <: Idiom: Type,
      N <: NamingStrategy: Type,
      Ctx <: Context[_, _],
      Res: Type
  ](quotedRaw: Expr[Quoted[BatchAction[A]]], batchContextOperation: Expr[ContextOperation[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res]])(using Quotes, Type[Ctx]):
    import quotes.reflect._

    val topLevelQuat = QuatMaking.ofType[T]

    def extractionBehavior: BatchExtractBehavior =
      Type.of[A] match
        case '[QAC[I, Nothing]] => ExtractBehavior.Skip
        case '[QAC[I, T]] =>
          if (!(TypeRepr.of[T] =:= TypeRepr.of[Any]))
            ExtractBehavior.ExtractWithReturnAction
          else
            ExtractBehavior.Skip
        case _ =>
          report.throwError(s"Could not match type type of the quoted operation: ${io.getquill.util.Format.TypeOf[A]}")

    def prepareLifts(): (ast.CaseClass, List[Expr[InjectableEagerPlanter[_, PrepareRow, Session]]]) =
      // Use some custom functionality in the lift macro to prepare the case class an injectable lifts
      // e.g. if T is Person(name: String, age: Int) and we do liftQuery(people:List[Person]).foreach(p => query[Person].insertValue(p))
      // Then:
      //   ast = CaseClass(name -> lift(UUID1), age -> lift(UUID2))
      //   lifts = List(InjectableEagerLift(p.name, UUID1), InjectableEagerLift(p.age, UUID2))
      val (caseClassAst, perRowLifts) = LiftMacro.liftInjectedProduct[I, PrepareRow, Session]

      // println(s"Case class AST: ${io.getquill.util.Messages.qprint(caseClassAst)}")
      // println("========= CaseClass =========\n" + io.getquill.util.Messages.qprint(caseClassAst))
      // Assuming that all lifts of the batch query are injectable

      // Verify that all of the lists are InjectableEagerPlanterExpr
      // TODO Need to examine this assumption
      // Maybe if user does liftQuery(people).foreach(p => query[Person].insert(_.name -> lift(p.name), _.age -> lift(123)))
      // then this won't be the case because the latter lift is not injectable i.e. it comes directly from the entity
      perRowLifts.foreach {
        case PlanterExpr.Uprootable(expr @ InjectableEagerPlanterExpr(_, _, _)) => expr
        case PlanterExpr.Uprootable(expr) =>
          report.throwError(s"wrong kind of uprootable ${(expr)}")
        case other => report.throwError(s"The lift expression ${Format(Printer.TreeStructure.show(other.asTerm))} is not valid for batch queries because it is not injectable")
      }
      (caseClassAst, perRowLifts)
    end prepareLifts

    /**
     * (TODO need to fix querySchema with batch usage i.e. liftQuery(people).insert(p => querySchema[Person](...).insertValue(p))
     * Create a quotation with the elaborated entity
     * e.g. given    liftQuery(people).foreach(p => query[Person].insert[Person](p))
     * then create a liftQuery(people).foreach(p => query[Person].insert[Person](_.name -> lift(p.name), _.age -> lift(p.age)))
     */
    def expandQuotation(actionQueryAstExpr: Expr[Ast], batchActionType: BatchActionType, perRowLifts: List[Expr[InjectableEagerPlanter[_, PrepareRow, Session]]]) =
      batchActionType match
        case BatchActionType.Insert => '{ Quoted[Insert[I]]($actionQueryAstExpr, ${ Expr.ofList(perRowLifts) }, Nil) }
        case BatchActionType.Update => '{ Quoted[Update[I]]($actionQueryAstExpr, ${ Expr.ofList(perRowLifts) }, Nil) }
        // We need lifts for 'Delete' because it could have a WHERE clause
        case BatchActionType.Delete => '{ Quoted[Delete[I]]($actionQueryAstExpr, ${ Expr.ofList(perRowLifts) }, Nil) }

    val quoted = quotedRaw.asTerm.underlyingArgument.asExpr

    def applyDynamic(): Expr[Res] =
      val (caseClass, perRowLifts) = prepareLifts()
      val caseClassExpr = Lifter.NotSerializingAst.caseClass(caseClass)
      val perRowLiftsExpr = Expr.ofList(perRowLifts)
      val extractionBehaviorExpr = Expr(extractionBehavior)
      val extractor = MakeExtractor[ResultRow, Session, T, T].dynamic(identityConverter, extractionBehavior)

      '{
        DynamicBatchQueryExecution.apply[I, T, A, ResultRow, PrepareRow, Session, D, N, Ctx, Res](
          $quotedRaw,
          $batchContextOperation,
          $caseClassExpr,
          $perRowLiftsExpr,
          $extractionBehaviorExpr,
          $extractor,
          // / For the sake of viewing/debugging the quat macro code it is better not to serialize it here
          ${ Lifter.NotSerializing.quat(topLevelQuat) }
        )
      }

    end applyDynamic

    enum ExpansionType:
      case Entities(entities: Expr[Iterable[_]])
      case Values(values: Expr[List[Any]], encoder: Expr[GenericEncoder[Any, PrepareRow, Session]])

    def apply(): Expr[Res] =
      UntypeExpr(quoted) match
        case QuotedExpr.UprootableWithLifts(QuotedExpr(quoteAst, _, _), planters) =>
          val unliftedAst = Unlifter(quoteAst)

          val comps = BatchStatic[I, PrepareRow, Session](unliftedAst, planters, extractionBehavior)
          val expandedQuotation = expandQuotation(Lifter(comps.actionQueryAst), comps.batchActionType, comps.perRowLifts)

          def expandLiftQueryMembers(filteredPerRowLifts: List[PlanterExpr[?, ?, ?]], entities: Expr[Iterable[?]]) =
            '{
              $entities.map(entity =>
                ${
                  // Since things like returningGenerated can exclude lifts...
                  //   For example:
                  //   query[Person].insert(_.id -> lift(1), _.name -> lift("Joe")).returningGenerated(_.id))
                  //   becomes something like Quoted(query[Person].insert(_.id -> lift(A), _.name -> lift(B)).returningGenerated(_.id)), lifts: List(ScalarTag(A, 1), ScalarTag(B, "Joe")))
                  //   but since we are excluding the person.id column (this is done in the transformation phase NormalizeReturning which is in SqlNormalization in the quill-sql-portable module)
                  //   actually we only want only the ScalarTag(B) so we need to get the list of lift tags (in tokens) once the Dialect has serialized the query
                  //   which correctly order the list of lifts. A similar issue happens with insertMeta and updateMeta.
                  // we need a pre-filtered, and ordered list of lifts. The StaticTranslationMacro interanally has done that so we can take the lifts from there although they need to be casted.
                  // This is safe because they are just the lifts taht we have already had from the `injectableLifts` list
                  // TODO If all the lists are not InjectableEagerPlanterExpr, then we need to find out which ones are not and not inject them
                  val injectedLifts = filteredPerRowLifts.asInstanceOf[List[InjectableEagerPlanterExpr[_, _, _]]].map(lift => lift.inject('entity))
                  val injectedLiftsExpr = Expr.ofList(injectedLifts)
                  // val prepare = '{ (row: PrepareRow, session: Session) => LiftsExtractor.apply[PrepareRow, Session]($injectedLiftsExpr, row, session) }
                  // prepare
                  injectedLiftsExpr
                }
              )
            }

          StaticTranslationMacro[I, T, D, N](expandedQuotation, ElaborationBehavior.Skip, topLevelQuat) match
            case Some(state @ StaticState(query, filteredPerRowLifts, _, _)) =>
              // create an extractor for returning actions
              val extractor = MakeExtractor[ResultRow, Session, T, T].static(state, identityConverter, extractionBehavior)

              // In an expression we could have a whole bunch of different lifts
              // liftQuery([Person1, Person2 <- these are EagerEntitiesPlanterExpr])
              //    .filter(p => p.id == lift(somethingElse) <- another lift expression)
              // etc...
              // So we need to go through all of them and expand
              // For example, say that we have:
              //   liftQuery([Joe, Jim]).foreach(p => query[Person].filter(p => p.id == lift(somethingElse)))
              // That means our lifts need to be:
              //   lift(Joe.name), lift(Joe.age), lift(somethingElse)
              //   lift(Jim.name), lift(Jim.age), lift(somethingElse)
              //
              // So first we expland the primary planter list into a list-of lists. The add all additional lifts
              // into each list. We are assuming that the primary planter (i.e. the liftQuery thing) is the 1st in the in the batch query
              val primaryPlanterLifts =
                comps.primaryPlanter match
                  case BatchStatic.PlanterKind.PrimaryEntitiesList(entitiesPlanter) =>
                    val exp = expandLiftQueryMembers(filteredPerRowLifts, entitiesPlanter.expr)
                    '{ $exp.toList }

                  case BatchStatic.PlanterKind.PrimaryScalarList(scalarsPlanter) =>
                    val exp = expandLiftQueryMembers(filteredPerRowLifts, scalarsPlanter.expr)
                    '{ $exp.toList }

              // At this point here is waht the lifts look like:
              //   List(
              //     List(lift(Joe.name), lift(Joe.age))
              //     List(lift(Jim.name), lift(Jim.age))
              //   )
              // We need to make them into:
              //   List(
              //     List(lift(Joe.name), lift(Joe.age)), lift(somethingElse) <- per-entity lifts of Joe
              //     List(lift(Jim.name), lift(Jim.age)), lift(somethingElse) <- per-entity lifts of Jim
              //   )
              val otherPlanters =
                Expr.ofList(
                  comps.categorizedPlanters.drop(1).map {
                    case BatchStatic.PlanterKind.Other(planter: EagerListPlanterExpr[_, _, _]) =>
                      planter.asInstanceOf[EagerListPlanterExpr[?, ?, ?]].plant
                    case other =>
                      report.throwError(s"Invalid planter: ${other}")
                  }
                )
              val combinedPlanters =
                '{ $primaryPlanterLifts.map(perEntityPlanters => perEntityPlanters ++ $otherPlanters) }

              // println(s"====================== PreparesList: ${Format.Expr(combinedPlanters)} =================")

              val prepares = '{
                $combinedPlanters.map(perRowList =>
                  (row: PrepareRow, session: Session) =>
                    LiftsExtractor.apply[PrepareRow, Session](perRowList, row, session)
                )
              }

              '{
                $batchContextOperation.execute(ContextOperation.Argument(${ Expr(query.basicQuery) }, $prepares.toArray, $extractor, ExecutionInfo(ExecutionType.Static, ${ Lifter(state.ast) }, ${ Lifter.quat(topLevelQuat) }), None))
              }

            case None =>
              // TODO report via trace debug
              // report.warning(s"Could not create static state from the query: ${Format.Expr(expandedQuotation)}")
              applyDynamic()

        case _ =>
          // TODO report via trace debug
          // report.warning(s"Batch actions must be static quotations. Found: ${Format.Expr(quoted)}", quoted)
          applyDynamic()
    end apply

  end RunQuery

  inline def apply[
      I,
      T,
      A <: QAC[I, T] with Action[I],
      ResultRow,
      PrepareRow,
      Session,
      D <: Idiom,
      N <: NamingStrategy,
      Ctx <: Context[_, _],
      Res
  ](inline quoted: Quoted[BatchAction[A]], ctx: ContextOperation[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res]) =
    ${ applyImpl[I, T, A, ResultRow, PrepareRow, Session, D, N, Ctx, Res]('quoted, 'ctx) }

  def applyImpl[
      I: Type,
      T: Type,
      A <: QAC[I, T] with Action[I]: Type,
      ResultRow: Type,
      PrepareRow: Type,
      Session: Type,
      D <: Idiom: Type,
      N <: NamingStrategy: Type,
      Ctx <: Context[_, _],
      Res: Type
  ](quoted: Expr[Quoted[BatchAction[A]]], ctx: Expr[ContextOperation[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res]])(using Quotes, Type[Ctx]): Expr[Res] =
    new RunQuery[I, T, A, ResultRow, PrepareRow, Session, D, N, Ctx, Res](quoted, ctx).apply()

end BatchQueryExecution

object BatchStatic:
  case class Components[PrepareRow, Session](
      actionQueryAst: Ast,
      batchActionType: BatchActionType,
      perRowLifts: List[Expr[InjectableEagerPlanter[?, PrepareRow, Session]]],
      categorizedPlanters: List[PlanterKind],
      primaryPlanter: PlanterKind.PrimaryEntitiesList | PlanterKind.PrimaryScalarList
  )

  sealed trait PlanterKind
  object PlanterKind:
    case class PrimaryEntitiesList(planter: EagerEntitiesPlanterExpr[?, ?, ?]) extends PlanterKind
    case class PrimaryScalarList(planter: EagerListPlanterExpr[?, ?, ?]) extends PlanterKind
    case class Other(planter: PlanterExpr[?, ?, ?]) extends PlanterKind

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
  def apply[I: Type, PrepareRow: Type, Session: Type](ast: Ast, planters: List[PlanterExpr[?, ?, ?]], extractionBehavior: BatchQueryExecutionModel.BatchExtractBehavior)(using Quotes) =
    import quotes.reflect._
    val (primaryPlanter, categorizedPlanters) =
      planters.foldLeft((Option.empty[PlanterKind.PrimaryEntitiesList | PlanterKind.PrimaryScalarList], List.empty[PlanterKind])) {
        case ((None, list), planter: EagerEntitiesPlanterExpr[?, ?, ?]) =>
          val planterKind = PlanterKind.PrimaryEntitiesList(planter)
          (Some(planterKind), list :+ planterKind)
        case ((None, list), planter: EagerListPlanterExpr[?, ?, ?]) =>
          val planterKind = PlanterKind.PrimaryScalarList(planter)
          (Some(planterKind), list :+ planterKind)
        case ((primary @ Some(_), list), planter) =>
          (primary, list :+ PlanterKind.Other(planter))
        // this means we haven't found the primary planter yet (don't think this can happen because nothing can be before liftQuery), keep going
        case ((primary @ None, list), planter) =>
          report.throwError("Invalid planter traversal")
      } match {
        case (Some(primary), categorizedPlanters) => (primary, categorizedPlanters)
        case (None, _)                            => report.throwError(s"Could not find an entities list-lift (i.e. liftQuery(entities/scalars) in liftQuery(...).foreach()) in lifts: ${planters.map(p => Format.Expr(p.plant))}")
      }

    // TODO check that there are no EagerEntitiesPlanterExpr other than in the primary planter
    val (actionQueryAst, batchActionType, perRowLifts) =
      primaryPlanter match {
        // In the case of liftQuery(entities)
        case PlanterKind.PrimaryEntitiesList(planter) =>
          planter.tpe match
            case '[t] => PrepareLiftQueryComponents[t, I, PrepareRow, Session](ast, extractionBehavior)

        // In the case of liftQuery(scalars)
        // Note, we could have potential other liftQuery(scalars) later in the query for example:
        // liftQuery(List("Joe","Jack","Jill")).foreach(query[Person].filter(name => liftQuery(1,2,3 /*ids of Joe,Jack,Jill respectively*/).contains(p.id)).update(_.name -> name))
        // Therefore we cannot assume that there is only one
        case PlanterKind.PrimaryScalarList(planter) =>
          planter.tpe match
            case '[t] => PrepareLiftQueryComponents[t, I, PrepareRow, Session](ast, extractionBehavior)
      }
    Components[PrepareRow, Session](actionQueryAst, batchActionType, perRowLifts, categorizedPlanters, primaryPlanter)
  end apply

end BatchStatic

object PrepareLiftQueryComponents:
  def apply[LiftQueryT: Type, I: Type, PrepareRow: Type, Session: Type](ast: Ast, extractionBehavior: BatchQueryExecutionModel.BatchExtractBehavior)(using Quotes) =
    import quotes.reflect._
    val (foreachReplacementAst, perRowLifts) = prepareLiftQueryLifts[LiftQueryT, PrepareRow, Session]
    val (actionQueryAst, batchActionType) =
      PrepareBatchComponents[I, PrepareRow](ast, foreachReplacementAst, extractionBehavior) match
        case Right(value) => value
        case Left(error) =>
          report.throwError(error)
    (actionQueryAst, batchActionType, perRowLifts)

  private def prepareLiftQueryLifts[LiftQueryT: Type, PrepareRow: Type, Session: Type](using Quotes): (ast.Ast, List[Expr[InjectableEagerPlanter[_, PrepareRow, Session]]]) =
    import quotes.reflect._
    // Use some custom functionality in the lift macro to prepare the case class an injectable lifts
    // e.g. if T is Person(name: String, age: Int) and we do liftQuery(people:List[Person]).foreach(p => query[Person].insertValue(p))
    // Then:
    //   ast = CaseClass(name -> lift(UUID1), age -> lift(UUID2))  // NOTE: lift in the AST means a ScalarTag
    //   lifts = List(InjectableEagerLift(p.name, UUID1), InjectableEagerLift(p.age, UUID2))
    // e.g. if T is String and we do liftQuery(people:List[String]).foreach(p => query[Person].insertValue(Person(p, 123)))
    // Then:
    //   ast = lift(UUID1)  // I.e. ScalarTag(UUID1) since lift in the AST means a ScalarTag
    //   lifts = List(InjectableEagerLift(p, UUID1))
    val (ast, fieldLifts) =
      QuatMaking.ofType[LiftQueryT] match
        case _: Quat.Product =>
          LiftMacro.liftInjectedProduct[LiftQueryT, PrepareRow, Session]
        case other =>
          val (ast, lift) = LiftMacro.liftInjectedScalar[LiftQueryT, PrepareRow, Session]
          (ast, List(lift))

    // println(s"Case class AST: ${io.getquill.util.Messages.qprint(caseClassAst)}")
    // println("========= CaseClass =========\n" + io.getquill.util.Messages.qprint(caseClassAst))
    // Assuming that all lifts of the batch query are injectable

    fieldLifts.foreach {
      case PlanterExpr.Uprootable(expr @ InjectableEagerPlanterExpr(_, _, _)) => expr
      case PlanterExpr.Uprootable(expr) =>
        report.throwError(s"wrong kind of uprootable ${(expr)}")
      case other => report.throwError(s"The lift expression ${Format(Printer.TreeStructure.show(other.asTerm))} is not valid for batch queries because it is not injectable")
    }
    (ast, fieldLifts)
  end prepareLiftQueryLifts
end PrepareLiftQueryComponents
