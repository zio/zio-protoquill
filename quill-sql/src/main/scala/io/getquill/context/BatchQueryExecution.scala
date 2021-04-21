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
import io.getquill.ast.ScalarTag
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

trait BatchContextOperation[T, A <: Action[_], D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Res](val idiom: D, val naming: N) {
  def execute(sql: String, prepare: List[PrepareRow => (List[Any], PrepareRow)], executionType: ExecutionType): Res
}

object BatchQueryExecution:
  class RunQuery[
    T: Type,
    A <: Action[_] with QAC[_, _]: Type,
    ResultRow: Type,
    PrepareRow: Type,
    D <: Idiom: Type,
    N <: NamingStrategy: Type,
    Res: Type
  ](quotedRaw: Expr[Quoted[BatchAction[A]]],
    batchContextOperation: Expr[BatchContextOperation[T, A, D, N, PrepareRow, ResultRow, Res]])(using val qctx: Quotes):
    import quotes.reflect._
    val quoted = quotedRaw.asTerm.underlyingArgument.asExpr
    def apply(): Expr[Res] =
      Uninline(UntypeExpr(quoted)) match
        case QuotedExpr.UprootableWithLifts(QuotedExpr(ast, _, _), planters) =>
          val iterableExpr =
            planters match
              case List(EagerEntitiesPlanterExpr(_, iterableExpr)) => iterableExpr
              case _ => report.throwError(s"Invalid liftQuery clause: ${planters}. Must be a single EagerEntitiesPlanter", quoted)

          val insertEntity = {
            // putting this in a block since I don't want to externally import these packages
            import io.getquill.ast._
            Unlifter(ast) match
              case Foreach(_, _, Insert(entity: Entity, _)) => entity
              case other => report.throwError(s"Malformed batch entity: ${other}. Batch insertion entities must have the form Insert(Entity, Nil: List[Assignment])")
          }

          import io.getquill.metaprog.InjectableEagerPlanterExpr

          // Use some custom functionality in the lift macro to prepare the case class an injectable lifts
          // e.g. if T is Person(name: String, age: Int) and we do liftQuery(people:List[Person]).foreach(p => query[Person].insert(p))
          // ast = CaseClass(name -> lift(UUID1), age -> lift(UUID2))
          // lifts = List(InjectableEagerLift(p.name, UUID1), InjectableEagerLift(p.age, UUID2))
          val (caseClassAst, rawLifts) = LiftMacro.liftInjectedProduct[T, PrepareRow]
          // Assuming that all lifts of the batch query are injectable
          val injectableLifts =
            rawLifts.map {
              case PlanterExpr.Uprootable(expr @ InjectableEagerPlanterExpr(_, _, _)) => expr
              case PlanterExpr.Uprootable(expr) =>
                report.throwError(s"wrong kind of uprootable ${(expr)}")
              case other => report.throwError(s"The lift expression ${Format.Expr(other)} is not valid for batch queries because it is not injectable")
            }

          // Once we have that, use the Insert macro to generate a correct insert clause. The insert macro
          // should summon a schemaMeta if needed (and account for querySchema age)
          // (TODO need to fix querySchema with batch usage i.e. liftQuery(people).insert(p => querySchema[Person](...).insert(p))
          val insertQuotation = InsertUpdateMacro.createFromPremade[T](insertEntity, caseClassAst, rawLifts)
          StaticTranslationMacro.applyInner[T, Nothing, D, N](insertQuotation) match
            case Some(StaticState(queryString, _, _)) =>
              val prepares =
                '{ $iterableExpr.map(elem => ${
                  val injectedLifts = injectableLifts.map(lift => lift.inject('elem))
                  val injectedLiftsExpr = Expr.ofList(injectedLifts)
                  val prepare = '{ (row: PrepareRow) => LiftsExtractor.apply[PrepareRow]($injectedLiftsExpr, row) }
                  prepare
                }) }
              '{ $batchContextOperation.execute(${Expr(queryString)}, $prepares.toList, ExecutionType.Static) }

            case None =>
              report.throwError(s"Could not create static state from the query: ${Format.Expr(insertQuotation)}")

        case _ =>
          report.throwError(s"Batch actions must be static quotations. Found: ${Format.Expr(quoted)}", quoted)

  end RunQuery

  inline def apply[
    T,
    A <: Action[_] with QAC[_, _],
    ResultRow,
    PrepareRow,
    D <: Idiom,
    N <: NamingStrategy,
    Res
  ](inline quoted: Quoted[BatchAction[A]], ctx: BatchContextOperation[T, A, D, N, PrepareRow, ResultRow, Res]) =
    ${ applyImpl[T, A, ResultRow, PrepareRow, D, N, Res]('quoted, 'ctx) }

  def applyImpl[
    T: Type,
    A <: Action[_] with QAC[_, _]: Type,
    ResultRow: Type,
    PrepareRow: Type,
    D <: Idiom: Type,
    N <: NamingStrategy: Type,
    Res: Type
  ](quoted: Expr[Quoted[BatchAction[A]]],
    ctx: Expr[BatchContextOperation[T, A, D, N, PrepareRow, ResultRow, Res]])(using qctx: Quotes): Expr[Res] =
    new RunQuery[T, A, ResultRow, PrepareRow, D, N, Res](quoted, ctx).apply()

end BatchQueryExecution
