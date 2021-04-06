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
import io.getquill.metaprog.Extractors
import io.getquill.BatchAction
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._

trait BatchContextOperation[T, D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Res](val idiom: D, val naming: N) {
  def execute(sql: String, prepare: List[PrepareRow], extractor: Option[ResultRow => T], executionType: ExecutionType): Res
}

object BatchQueryExecution:
  class RunQuery[
    T: Type,
    ResultRow: Type, 
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quoted: Expr[BatchAction[Action[T]]],
    batchContextOperation: Expr[BatchContextOperation[T, D, N, PrepareRow, ResultRow, Res]])(using val qctx: Quotes) 
  extends Extractors:
    import quotes.reflect._

    def apply(): Expr[Res] = 
      QuotationLotExpr.Unquoted(UntypeExpr(quoted)) match
        case Uprootable(_, ast, _, _, queryLifts, _) => 
          queryLifts match
            case List(PlanterExpr.Uprootable(EagerEntitiesPlanterExpr(uid, liftsIterator))) =>
              ???

            case _ =>
              report.throwError(s"Invalid liftQuery clause: ${queryLifts}. Must be a single EagerEntitiesPlanter", quoted)

        case _ =>
          report.throwError(s"Batch actions must be static quotations. Found: ${quoted}", quoted)

  end RunQuery

  inline def apply[
    T, 
    ResultRow, 
    PrepareRow, 
    D <: Idiom, 
    N <: NamingStrategy, 
    Res
  ](inline quoted: BatchAction[Action[T]], ctx: BatchContextOperation[T, D, N, PrepareRow, ResultRow, Res]) = 
    ${ applyImpl('quoted, 'ctx) }
  
  def applyImpl[
    T: Type,
    ResultRow: Type,
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quoted: Expr[BatchAction[Action[T]]],
    ctx: Expr[BatchContextOperation[T, D, N, PrepareRow, ResultRow, Res]])(using qctx: Quotes): Expr[Res] =
    new RunQuery[T, ResultRow, PrepareRow, D, N, Res](quoted, ctx).apply()

end BatchQueryExecution
