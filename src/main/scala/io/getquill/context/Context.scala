package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.dsl.Dsl
//import io.getquill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import miniquill.dsl.EncodingDsl
import miniquill.quoter.Quoted
import miniquill.quoter.QueryMeta
import io.getquill.derived._
import miniquill.context.mirror.MirrorDecoders
import miniquill.context.mirror.Row
import miniquill.dsl.GenericDecoder
import miniquill.quoter.ScalarPlanter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import scala.quoted._
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotedExpr
import miniquill.quoter.ScalarPlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.Query

import io.getquill._

sealed trait ExecutionType
object ExecutionType {
  case object Dynamic extends ExecutionType
  case object Static extends ExecutionType
}


trait RunDsl[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy] { 
  context: Context[Dialect, Naming] =>

  inline def runQuery[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = 
    ${ RunDsl.runQueryImpl[T, ResultRow, PrepareRow, Dialect, Naming, Result[RunQueryResult[T]]]('quoted, 'this) }
}

object StaticExtractor {
  def apply[PrepareRowTemp](lifts: List[ScalarPlanter[_, _]], row: PrepareRowTemp) = {
    val (_, values, prepare) =
      lifts.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val newRow = 
            lift
            .asInstanceOf[ScalarPlanter[Any, PrepareRowTemp]]
            .encoder(idx, lift.value, row).asInstanceOf[PrepareRowTemp] // TODO since summoned encoders are casted
          (idx + 1, lift.value :: values, newRow)
      }
    (values, prepare)
  }
}


case class StaticState(query: String, lifts: Expr[List[miniquill.quoter.ScalarPlanter[?, ?]]])

object RunDynamic {

  import io.getquill.idiom.{ Idiom => Id }
  import io.getquill.{ NamingStrategy => Na }

  def apply[RawT, T, D <: Id, N <: Na](
    quoted: Quoted[Query[RawT]], 
    decoder: GenericDecoder[_, RawT], 
    converter: RawT => T,
    context: Context[D, N],
    ast: Ast // Expander.runtime[RawT](quoted.ast)
  ): context.Result[context.RunQueryResult[T]] = 
  {
    val idiom: D = context.idiom
    val naming: N = context.naming

    // println("Runtime Expanded Ast Is: " + ast)
    val lifts = quoted.lifts // quoted.lifts causes position exception
    val quotationVases = quoted.runtimeQuotes // quoted.runtimeQuotes causes position exception

    def spliceQuotations(ast: Ast): Ast =
      Transform(ast) {
        case v @ QuotationTag(uid) => 
          // When a quotation to splice has been found, retrieve it and continue
          // splicing inside since there could be nested sections that need to be spliced
          quotationVases.find(_.uid == uid) match {
            case Some(vase) => 
              spliceQuotations(vase.quoted.ast)
            case None =>
              throw new IllegalArgumentException(s"Quotation vase with UID ${uid} could not be found!")
          }
      }

    val expandedAst = spliceQuotations(ast)
      
    val (outputAst, stmt) = idiom.translate(expandedAst)(using naming)

    val (string, externals) =
      ReifyStatement(
        idiom.liftingPlaceholder,
        idiom.emptySetContainsToken,
        stmt,
        forProbing = false
      )

    // summon a decoder and a expander (as well as an encoder) all three should be provided by the context
      // summonFrom {
      //   // TODO Implicit summoning error
      //   case decoder: Decoder[T] => decoder
      // }
    val extractor = (r: context.ResultRow) => converter(decoder.asInstanceOf[GenericDecoder[context.ResultRow, RawT]].apply(1, r))
    context.executeQuery(string, null, extractor, ExecutionType.Dynamic)
  }
}

object RunDsl {

  import io.getquill.idiom.{ Idiom => Id }
  import io.getquill.{ NamingStrategy => Na }

  def runWithMeta[T: Type, RawT: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    quoted: Expr[Quoted[Query[T]]],
    ctx: Expr[Context[D, N]]
  )(using qctx: Quotes): Expr[Res] = 
  {
    import qctx.reflect._
    val (queryRawT, converter, staticStateOpt) = QueryMetaExtractor.applyImpl[T, RawT, D, N](quoted, ctx)

    val output =
      staticStateOpt match {
        case Some(staticState) =>
          executeQueryStatic[T, RawT, ResultRow, PrepareRow, D, N, Res](ctx, staticState, converter)

        case None => 
          println("========== Expanding Dynamically ===========")
          val decoder = summonDecoderOrThrow[ResultRow, RawT]
          val quotedAst = '{ $quoted.ast }
          // Is the expansion on T or RawT, need to investigate
          val expandedAst = Expander.runtimeImpl[T](quotedAst)
          println(s"Expanded Ast: " + pprint.apply(Term.of(expandedAst)))
          '{ 
            RunDynamic.apply[RawT, T, D, N]($queryRawT, $decoder, $converter, $ctx, $expandedAst).asInstanceOf[Res]
          }
      }

    output
  }

  def executeQueryStatic[T: Type, RawT: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    ctx: Expr[Context[D, N]],
    staticState: StaticState,
    converter: Expr[RawT => T]
  )(using qctx: Quotes): Expr[Res] = 
  {
    import qctx.reflect._

    val ctxTerm = Term.of(ctx)
    val ctxClass = ctxTerm.tpe.widen.classSymbol.get
    val executeQuery = ctxClass.methods.filter(f => f.name == "executeQuery").head

    val (query, lifts) = (staticState.query, staticState.lifts)
    // TODO if there's  a type that gets summoned the T here is is that someR
    val decoder = summonDecoderOrThrow[ResultRow, RawT]

    val extractor = '{ (r: ResultRow) => $converter.apply($decoder.apply(1, r)) }
    val prepare = '{ (row: PrepareRow) => StaticExtractor.apply[PrepareRow]($lifts, row) }

    val applyExecuteQuery =
      Apply(
        TypeApply(Select(ctxTerm, executeQuery), List(TypeTree.of[T])),
        List(Term.of(Expr(query)), Term.of('{null}), Term.of(extractor), Term.of('{ExecutionType.Static}))
      )
    val res = applyExecuteQuery.asExprOf[Res]
    res

    // =================== This Doesn't work ==================
    //val output = '{ $ctx.executeQuery(${Expr(query)}, null, $extractor, ExecutionType.Static) }
  }

  def summonDecoderOrThrow[ResultRow: Type, T: Type](using qctx: Quotes): Expr[GenericDecoder[ResultRow, T]] = {
    import qctx.reflect._
    Expr.summon[GenericDecoder[ResultRow, T]] match {
      case Some(decoder) => decoder
      case None => report.throwError("Decoder could not be summoned")
    }
  }
  
  def runQueryImpl[T: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    quoted: Expr[Quoted[Query[T]]],
    ctx: Expr[Context[D, N]]
  )(using qctx: Quotes): Expr[Res] = 
  {
    import qctx.reflect._
    val tmc = new miniquill.parser.TastyMatchersContext
    import tmc._

    val rowType =
      Expr.summon[QueryMeta[T, _]] match {
        case Some(expr) =>
          // println("Summoned! " + expr.show)
          UntypeExpr(expr) match {
            case '{ QueryMeta.apply[k, n]($one, $two, $uid) } => Some('[n])
            case _ => report.throwError("Summoned Query Meta But Could Not Get Type")
          }
        case None => None
      }

    val output =
      rowType match {
        case Some(rowRepr) =>
          rowRepr match {
            case '[rawT] => runWithMeta[T, rawT, ResultRow, PrepareRow, D, N, Res](quoted, ctx)
          }
        case None =>
          StaticTranslationMacro.applyInner[T, D, N](quoted) match {
            case Some(staticState) =>
              executeQueryStatic[T, T, ResultRow, PrepareRow, D, N, Res](ctx, staticState, '{ (t:T) => t })

            case None => 
              println("========== Expanding Dynamically ===========")
              val decoder = summonDecoderOrThrow[ResultRow, T]
              val quotedAst = '{ $quoted.ast }
              val expandedAst = Expander.runtimeImpl[T](quotedAst)
              '{ 
                RunDynamic.apply[T, T, D, N]($quoted, $decoder, (t: T) => t, $ctx, $expandedAst).asInstanceOf[Res]
              }
              
          }
      }
    output
  }
}

// TODO Needs to be portable (i.e. plug into current contexts when compiled with Scala 3)
trait Context[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy]
extends RunDsl[Dialect, Naming]
with EncodingDsl
//  extends Closeable
{ self =>
  

  implicit inline def autoDecoder[T]:Decoder[T] = GenericDecoder.derived

  type PrepareRow
  type ResultRow

  type Result[T]
  type RunQuerySingleResult[T]
  type RunQueryResult[T]
  type RunActionResult
  type RunActionReturningResult[T]
  type RunBatchActionResult
  type RunBatchActionReturningResult[T]
  type Session

  type Prepare = PrepareRow => (List[Any], PrepareRow)
  type Extractor[T] = ResultRow => T

  case class BatchGroup(string: String, prepare: List[Prepare])
  case class BatchGroupReturning(string: String, returningBehavior: ReturnAction, prepare: List[Prepare])

  //def probe(statement: String): Try[_]

  def idiom: Dialect
  def naming: Naming

  // todo add 'prepare' i.e. encoders here
  def executeQuery[T](sql: String, prepare: Prepare, extractor: Extractor[T], executionType: ExecutionType): Result[RunQueryResult[T]]

  val identityPrepare: Prepare = (Nil, _)
  val identityExtractor = identity[ResultRow] _

  

  inline def lift[T](inline vv: T): T = 
    ${ LiftMacro[T, PrepareRow]('vv) }

  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = 
    runQuery[T](quoted)


}
