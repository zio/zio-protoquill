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
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotedExpr
import miniquill.quoter.ScalarPlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.Query
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import miniquill.parser.TastyMatchers

trait ContextAction[T, D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Res] {
  def idiom: D
  def naming: N
  def execute(sql: String, prepare: PrepareRow => (List[Any], PrepareRow), extractor: ResultRow => T, executionType: ExecutionType): Res
}

/**
 * Drives execution of Quoted blocks i.e. Queries etc... from the context.
 */
object QueryExecution:

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
            case '{ QueryMeta.apply[k, n]($one, $two, $uid) } => Some(Type.of[n])
            case _ => report.throwError("Summoned Query Meta But Could Not Get Type")
          }
        case None => None
      }
  }

  class RunQuery[
    T: Type, 
    ResultRow: Type, 
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quoted: Expr[Quoted[Query[T]]], 
    contextAction: Expr[ContextAction[T, D, N, PrepareRow, ResultRow, Res]])(using val qctx: Quotes) 
  extends SummonHelper[ResultRow] 
    with QueryMetaHelper[T] 
    with TastyMatchers:
    
    import qctx.reflect._

    /** Run a query with a given QueryMeta given by the output type RawT and the conversion RawT back to T */
    def runWithMeta[RawT: Type]: Expr[Res] =
      val (queryRawT, converter, staticStateOpt) = QueryMetaExtractor.applyImpl[T, RawT, D, N](quoted)
      staticStateOpt match {
        case Some(staticState) =>
          executeStatic[RawT](staticState, converter)
        case None => 
          executeDynamic[RawT, Query](queryRawT, converter)
      }
    

    def executeDynamic[RawT: Type, Q[_]: Type](query: Expr[Quoted[Q[RawT]]], converter: Expr[RawT => T]) =
      val decoder = summonDecoderOrThrow[RawT]
      // Is the expansion on T or RawT, need to investigate
      val expandedAst = Expander.runtimeImpl[T]('{ $query.ast })

      val extractor = '{ (r: ResultRow) => $converter.apply($decoder.apply(1, r)) }
      val prepare = '{ (row: PrepareRow) => LiftsExtractor.apply[PrepareRow]($query.lifts, row) }

      // TODO What about when an extractor is not neededX
      '{  RunDynamicExecution.apply[RawT, T, Q, D, N, PrepareRow, ResultRow, Res]($query, $contextAction, $prepare, $extractor, $expandedAst) }
    

    /** 
     * Execute static query via ctx.executeQuery method given we have the ability to do so 
     * i.e. have a staticState 
     */
    def executeStatic[RawT: Type](staticState: StaticState, converter: Expr[RawT => T]): Expr[Res] =    
      val StaticState(query, lifts) = staticState
      val decoder = summonDecoderOrThrow[RawT]

      val extractor = '{ (r: ResultRow) => $converter.apply($decoder.apply(1, r)) }
      val prepare = '{ (row: PrepareRow) => LiftsExtractor.apply[PrepareRow]($lifts, row) }

      // TODO What about when an extractor is not neededX
      // executeAction(query, prepare, extractor)
      '{ $contextAction.execute(${Expr(query)}, $prepare, $extractor, ExecutionType.Static) }


    /** Summon all needed components and run executeQuery method */
    def apply(): Expr[Res] =
      summonMetaIfExists match
        case Some(rowRepr) =>
          rowRepr match { case '[rawT] => runWithMeta[rawT] }
        case None =>
          StaticTranslationMacro.applyInner[Query, T, D, N](quoted) match 
            case Some(staticState) =>
              executeStatic[T](staticState, '{ (t:T) => t })
            case None => 
              executeDynamic(quoted, '{ (t: T) => t })


  end RunQuery

  inline def runQuery[
    T, 
    ResultRow, 
    PrepareRow, 
    D <: Idiom, 
    N <: NamingStrategy, 
    Res
  ](quoted: Quoted[Query[T]], ctx: ContextAction[T, D, N, PrepareRow, ResultRow, Res]) = 
    ${ runQueryImpl('quoted, 'ctx) }
  
  def runQueryImpl[
    T: Type, 
    ResultRow: Type, 
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quoted: Expr[Quoted[Query[T]]], 
    ctx: Expr[ContextAction[T, D, N, PrepareRow, ResultRow, Res]])(using qctx: Quotes): Expr[Res] =
    new RunQuery[T, ResultRow, PrepareRow, D, N, Res](quoted, ctx).apply()

end QueryExecution

/**
 * Drives dynamic execution from the Context
 */
object RunDynamicExecution:

  import io.getquill.idiom.{ Idiom => Idiom }
  import io.getquill.{ NamingStrategy => NamingStrategy }

  def apply[
    RawT, 
    T, 
    Q[_], 
    D <: Idiom, 
    N <: NamingStrategy, 
    PrepareRow, 
    ResultRow, 
    Res
  ](quoted: Quoted[Q[RawT]], 
    ctx: ContextAction[T, D, N, PrepareRow, ResultRow, Res],
    prepare: PrepareRow => (List[Any], PrepareRow),
    extractor: ResultRow => T,
    expandedAst: Ast
  ): Res = 
  {
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
    //val expandedAst = Expander.runtime[T](quoted.ast) // cannot derive a decoder for T
    val splicedAst = spliceQuotations(expandedAst)
      
    val (outputAst, stmt) = ctx.idiom.translate(splicedAst)(using ctx.naming)

    val (string, externals) =
      ReifyStatement(
        ctx.idiom.liftingPlaceholder,
        ctx.idiom.emptySetContainsToken,
        stmt,
        forProbing = false
      )

    ctx.execute(string, prepare, extractor, ExecutionType.Dynamic)
  }

end RunDynamicExecution