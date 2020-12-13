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

  inline def runTest[T](inline quoted: Quoted[T]): String = 
    ${ RunDslRet.runTestImpl[T, ResultRow, PrepareRow, Dialect, Naming, Result[RunQueryResult[T]]]('quoted, 'this) }
}

object LiftsExtractor {
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

/** Wrapper for components needed in order to execute a query during compile-time */
case class StaticState(query: String, lifts: Expr[List[miniquill.quoter.ScalarPlanter[?, ?]]])

/** Execute a query during runtime */
object RunDynamic {

  import io.getquill.idiom.{ Idiom => Id }
  import io.getquill.{ NamingStrategy => Na }

  def apply[RawT, T, D <: Id, N <: Na](
    quoted: Quoted[Query[RawT]], 
    decoder: GenericDecoder[_, RawT], 
    converter: RawT => T,
    context: Context[D, N],
    ast: Ast
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

    // Splice all quotation values back into the AST recursively, by this point these quotations are dynamic
    // which means that the compiler has not done the splicing for us. We need to do this ourselves. 
    val expandedAst = spliceQuotations(ast)
      
    val (outputAst, stmt) = idiom.translate(expandedAst)(using naming)

    val (string, externals) =
      ReifyStatement(
        idiom.liftingPlaceholder,
        idiom.emptySetContainsToken,
        stmt,
        forProbing = false
      )

    // TODO Finish dynamic prepareRow
    val extractor = (r: context.ResultRow) => converter(decoder.asInstanceOf[GenericDecoder[context.ResultRow, RawT]].apply(1, r))
    val prepare = (row: context.PrepareRow) => LiftsExtractor.apply[context.PrepareRow](lifts, row)
    context.executeQuery(string, prepare, extractor, ExecutionType.Dynamic)
  }
}





object RunDsl {

  import io.getquill.idiom.{ Idiom => Id }
  import io.getquill.{ NamingStrategy => Na }
  import miniquill.parser.TastyMatchers

  trait SummonHelper[ResultRow: Type] {
    implicit val qctx: Quotes
    import qctx.reflect._

    /** Summon decoder for a given Type and Row type (ResultRow) */
    def summonDecoderOrThrow[DecoderT: Type]: Expr[GenericDecoder[ResultRow, DecoderT]] = {
      Expr.summon[GenericDecoder[ResultRow, DecoderT]] match {
        case Some(decoder) => decoder
        case None => report.throwError("Decoder could not be summoned")
      }
    }
  }

  trait QueryMetaHelper[T: Type] extends TastyMatchers {
    // See if there there is a QueryMeta mapping T to some other type RawT
    def summonMetaIfExists =
      Expr.summon[QueryMeta[T, _]] match {
        case Some(expr) =>
          // println("Summoned! " + expr.show)
          UntypeExpr(expr) match {
            case '{ QueryMeta.apply[k, n]($one, $two, $uid) } => Some('[n])
            case _ => report.throwError("Summoned Query Meta But Could Not Get Type")
          }
        case None => None
      }
  }


  class RunQuery[T: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    quoted: Expr[Quoted[Query[T]]],
    ctx: Expr[Context[D, N]]
  )(using val qctx: Quotes) extends SummonHelper[ResultRow] with QueryMetaHelper[T] with TastyMatchers {
    import qctx.reflect._

    /** Run a query with a given QueryMeta given by the output type RawT and the conversion RawT back to T */
    def runWithMeta[RawT: Type]: Expr[Res] = {
      val (queryRawT, converter, staticStateOpt) = QueryMetaExtractor.applyImpl[T, RawT, D, N](quoted)
      staticStateOpt match {
        case Some(staticState) =>
          executeQueryStatic[RawT](staticState, converter)
        case None => 
          executeQueryDynamic[RawT](queryRawT, converter)
      }
    }

    def executeQueryDynamic[RawT: Type](query: Expr[Quoted[Query[RawT]]], converter: Expr[RawT => T]) = {
      val decoder: Expr[miniquill.dsl.GenericDecoder[ResultRow, RawT]] = summonDecoderOrThrow[RawT]
      val quotedAst = '{ $quoted.ast }
      val expandedAst = Expander.runtimeImpl[T](quotedAst)
      '{  RunDynamic.apply[RawT, T, D, N]($query, $decoder, $converter, $ctx, $expandedAst).asInstanceOf[Res] }
    }

    /** 
     * Execute static query via ctx.executeQuery method given we have the ability to do so 
     * i.e. have a staticState 
     */
    def executeQueryStatic[RawT: Type](staticState: StaticState, converter: Expr[RawT => T]): Expr[Res] = {      
      val executeQuery = summonContextMethod("executeQuery", ctx)
      val StaticState(query, lifts) = staticState
      val decoder = summonDecoderOrThrow[RawT]

      val extractor = '{ (r: ResultRow) => $converter.apply($decoder.apply(1, r)) }
      val prepare = '{ (row: PrepareRow) => LiftsExtractor.apply[PrepareRow]($lifts, row) }

      // executeQuery(query, prepare, extractor)
      val applyExecuteQuery =
        Apply(
          TypeApply(Select(Term.of(ctx), executeQuery), List(TypeTree.of[T])),
          List(Term.of(Expr(query)), Term.of(prepare), Term.of(extractor), Term.of('{ExecutionType.Static}))
        )
      applyExecuteQuery.asExprOf[Res]
    }

    /** Summon all needed components and run executeQuery method */
    def apply(): Expr[Res] = {
      summonMetaIfExists match {
        case Some(rowRepr) =>
          rowRepr match { case '[rawT] => runWithMeta[rawT] }
        case None =>
          StaticTranslationMacro.applyInner[Query, T, D, N](quoted) match {
            case Some(staticState) =>
              executeQueryStatic[T](staticState, '{ (t:T) => t })

            case None => 
              executeQueryDynamic(quoted, '{ (t: T) => t })
          }
      }
    }
  }
  
  def runQueryImpl[T: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    quoted: Expr[Quoted[Query[T]]],
    ctx: Expr[Context[D, N]]
  )(using qctx: Quotes): Expr[Res] = {
    new RunQuery[T, ResultRow, PrepareRow, D, N, Res](quoted, ctx).apply()
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

  inline def runAndTest[T](inline quoted: Quoted[T]): String = 
    runTest[T](quoted)
}
