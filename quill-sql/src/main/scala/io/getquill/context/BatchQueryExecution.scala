package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.generic.Dsl
//import io.getquill.util.Messages.fail
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
import io.getquill.generic.GenericEncoder
import io.getquill.Planter
import io.getquill.EagerPlanter
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

trait BatchContextOperation[I, T, A <: QAC[I, T] with Action[I], D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Res](val idiom: D, val naming: N) {
  def execute(sql: String, prepare: List[PrepareRow => (List[Any], PrepareRow)], extractor: Extraction[ResultRow, T], executionInfo: ExecutionInfo): Res
}

object BatchQueryExecution:

  private[getquill] enum BatchActionType:
    case Insert
    case Update
    case Delete

  private[getquill] object ActionEntity:
    def unapply(actionAst: Ast): Option[BatchActionType] =
      actionAst match
        case ast.Insert(entity, _) => Some(BatchActionType.Insert)
        case ast.Update(entity, assignments) => Some(BatchActionType.Update)
        case ast.Delete(entity) => Some(BatchActionType.Delete)
        case _ => None

  private[getquill] class RunQuery[
    I: Type,
    T: Type,
    A <: QAC[I, T] & Action[I]: Type,
    ResultRow: Type,
    PrepareRow: Type,
    D <: Idiom: Type,
    N <: NamingStrategy: Type,
    Res: Type
  ](quotedRaw: Expr[Quoted[BatchAction[A]]],
    batchContextOperation: Expr[BatchContextOperation[I, T, A, D, N, PrepareRow, ResultRow, Res]])(using val qctx: Quotes):
    import quotes.reflect._
    import Execution._

    val quoted = quotedRaw.asTerm.underlyingArgument.asExpr
    def apply(): Expr[Res] =
      UntypeExpr(quoted) match
        case QuotedExpr.UprootableWithLifts(QuotedExpr(quoteAst, _, _), planters) =>
          // isolate the list that went into the liftQuery i.e. the liftQuery(liftedList)
          val liftedList =
            planters match
              case List(EagerEntitiesPlanterExpr(_, liftedList)) => liftedList
              case _ => report.throwError(s"Invalid liftQuery clause: ${planters}. Must be a single EagerEntitiesPlanter", quoted)

          val extractionBehavior: ExtractBehavior.Skip.type | ExtractBehavior.ExtractWithReturnAction.type =
            Type.of[A] match
              case '[QAC[I, Nothing]] => ExtractBehavior.Skip
              case '[QAC[I, T]] =>
                if (!(TypeRepr.of[T] =:= TypeRepr.of[Any]))
                  ExtractBehavior.ExtractWithReturnAction
                else
                  ExtractBehavior.Skip
              case _ =>
                report.throwError(s"Could not match type type of the quoted operation: ${io.getquill.util.Format.TypeOf[A]}")

          val (foreachIdent, actionQueryAstRaw, batchActionType) = {
            // putting this in a block since I don't want to externally import these packages
            import io.getquill.ast._
            val unliftedAst = Unlifter(quoteAst)
            extractionBehavior match
              case ExtractBehavior.Skip =>
                unliftedAst match
                  // TODO In the actionQueryAst should we make sure to verify that an Entity exists?
                  case Foreach(_, foreachIdent, actionQueryAst @ ActionEntity(bType)) => (foreachIdent, actionQueryAst, bType)
                  case other => report.throwError(s"Malformed batch entity: ${io.getquill.util.Messages.qprint(other)}. Batch insertion entities must have the form Insert(Entity, Nil: List[Assignment])")

              case ExtractBehavior.ExtractWithReturnAction =>
                unliftedAst match
                  // Designed to Match:          liftQuery(...).foreach(p => query[Person].insert(...))
                  // Also Matches:               liftQuery(...).foreach(p => query[Person].filter(...).insert(...))
                  // but more generally matches: liftQuery(...).foreach(p => {stuff})
                  // TODO In the actionQueryAst should we make sure to verify that an Entity exists?
                  case Foreach(_, foreachIdent, actionQueryAst @ ReturningAction(ActionEntity(bType), id, body)) =>
                    actionQueryAst match
                      case _: Returning =>           (foreachIdent, actionQueryAst, bType)
                      case _: ReturningGenerated =>  (foreachIdent, actionQueryAst, bType)
                  case other => report.throwError(s"Malformed batch entity: ${other}. Batch insertion entities must have the form Returning/ReturningGenerated(Insert(Entity, Nil: List[Assignment]), _, _)")
          }

          // Use some custom functionality in the lift macro to prepare the case class an injectable lifts
          // e.g. if T is Person(name: String, age: Int) and we do liftQuery(people:List[Person]).foreach(p => query[Person].insert(p))
          // ast = CaseClass(name -> lift(UUID1), age -> lift(UUID2))
          // lifts = List(InjectableEagerLift(p.name, UUID1), InjectableEagerLift(p.age, UUID2))
          val (caseClassAst, rawLifts) = LiftMacro.liftInjectedProduct[I, PrepareRow]
          //println(s"Case class AST: ${io.getquill.util.Messages.qprint(caseClassAst)}")
          //println("========= CaseClass =========\n" + io.getquill.util.Messages.qprint(caseClassAst))
          // Assuming that all lifts of the batch query are injectable

          // The primary idea that drives batch query execution is the realization that you
          // can beta reduce out the foreach identifier replacing it with lift tags.
          // For example if we have something like:
          // actionQueryAstRaw: liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).update(_.name == p.name))
          // where Person(id: Int, name: String, age: Int)
          // all we need do do is to take the caseClassAst which is ast.CaseClass(p.id -> ScalarTag(A), p.name -> ScalarTag(B), p.age -> ScalarTag(C))
          // (and corresponding rawLifts (EagerPlanter(A), EagerPlanter(B), EagerPlanter(C)))
          // then do a beta reduction which will turn our actionQueryAstRaw into:
          // actionQueryAstRaw: liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == ScalarTag(A)).update(_.name == ScalarTag(B)))
          // this will ultimately yield a query that looks like: UPDATE Person SET name = ? WHERE id = ? and for each person entity
          // the corresponding values will be plugged in
          val actionQueryAst = BetaReduction(actionQueryAstRaw, foreachIdent -> caseClassAst)
          //println(s"==== Reduced AST: ${io.getquill.util.Messages.qprint(actionQueryAst)}")

          // Verify that all of the lists are InjectableEagerPlanterExpr
          // TODO Need to examine this assumption
          // Maybe if user does liftQuery(people).foreach(p => query[Person].insert(_.name -> lift(p.name), _.age -> lift(123)))
          // then this won't be the case because the latter lift is not injectable i.e. it comes directly from the entity
          rawLifts.foreach {
            case PlanterExpr.Uprootable(expr @ InjectableEagerPlanterExpr(_, _, _)) => expr
            case PlanterExpr.Uprootable(expr) =>
              report.throwError(s"wrong kind of uprootable ${(expr)}")
            case other => report.throwError(s"The lift expression ${Format(Printer.TreeStructure.show(other.asTerm))} is not valid for batch queries because it is not injectable")
          }

          // Once we have that, use the Insert macro to generate a correct insert clause. The insert macro
          // should summon a schemaMeta if needed (and account for querySchema age)
          // (TODO need to fix querySchema with batch usage i.e. liftQuery(people).insert(p => querySchema[Person](...).insert(p))

          // Create a quotation with the elaborated entity
          // e.g. given    liftQuery(people).foreach(p => query[Person].insert[Person](p))
          // then create a liftQuery(people).foreach(p => query[Person].insert[Person](_.name -> lift(p.name), _.age -> lift(p.age)))
          val quotation =
            batchActionType match
              case BatchActionType.Insert => '{ Quoted[Insert[I]](${Lifter(actionQueryAst)}, ${Expr.ofList(rawLifts)}, Nil) }
              case BatchActionType.Update => '{ Quoted[Update[I]](${Lifter(actionQueryAst)}, ${Expr.ofList(rawLifts)}, Nil) }
              // We need lifts for 'Delete' because it could have a WHERE clause
              case BatchActionType.Delete => '{ Quoted[Delete[I]](${Lifter(actionQueryAst)}, ${Expr.ofList(rawLifts)}, Nil) }

          StaticTranslationMacro.applyInner[I, T, D, N](quotation, ElaborationBehavior.Skip) match
            case Some(state @ StaticState(query, filteredLists, _)) =>
              // create an extractor for returning actions
              val extractor = MakeExtractor[ResultRow, T, T].static(state, identityConverter, extractionBehavior)

              val prepares =
                '{ $liftedList.map(elem => ${
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
                  val injectedLifts = filteredLists.asInstanceOf[List[InjectableEagerPlanterExpr[_, _]]].map(lift => lift.inject('elem))
                  val injectedLiftsExpr = Expr.ofList(injectedLifts)
                  val prepare = '{ (row: PrepareRow) => LiftsExtractor.apply[PrepareRow]($injectedLiftsExpr, row) }
                  prepare
                }) }
              '{ $batchContextOperation.execute(${Expr(query.basicQuery)}, $prepares.toList, $extractor, ExecutionInfo(ExecutionType.Static, ${Lifter(state.ast)})) }

            case None =>
              report.throwError(s"Could not create static state from the query: ${Format.Expr(quotation)}")

        case _ =>
          report.throwError(s"Batch actions must be static quotations. Found: ${Format.Expr(quoted)}", quoted)

  end RunQuery

  inline def apply[
    I,
    T,
    A <: QAC[I, T] with Action[I],
    ResultRow,
    PrepareRow,
    D <: Idiom,
    N <: NamingStrategy,
    Res
  ](inline quoted: Quoted[BatchAction[A]], ctx: BatchContextOperation[I, T, A, D, N, PrepareRow, ResultRow, Res]) =
    ${ applyImpl[I, T, A, ResultRow, PrepareRow, D, N, Res]('quoted, 'ctx) }

  def applyImpl[
    I: Type,
    T: Type,
    A <: QAC[I, T] with Action[I]: Type,
    ResultRow: Type,
    PrepareRow: Type,
    D <: Idiom: Type,
    N <: NamingStrategy: Type,
    Res: Type
  ](quoted: Expr[Quoted[BatchAction[A]]],
    ctx: Expr[BatchContextOperation[I, T, A, D, N, PrepareRow, ResultRow, Res]])(using qctx: Quotes): Expr[Res] =
    new RunQuery[I, T, A, ResultRow, PrepareRow, D, N, Res](quoted, ctx).apply()

end BatchQueryExecution
