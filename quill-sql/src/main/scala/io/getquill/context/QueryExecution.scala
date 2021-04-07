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
import io.getquill.Planter
import io.getquill.idiom.ReifyStatement
import io.getquill.Query
import io.getquill.Action
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.metaprog.Extractors
import io.getquill.BatchAction

trait ContextOperation[T, D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Res](val idiom: D, val naming: N) {
  def execute(sql: String, prepare: PrepareRow => (List[Any], PrepareRow), extractor: Option[ResultRow => T], executionType: ExecutionType): Res
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
        case None => report.throwError("Decoder could not be summoned during query execution")
      }
    }
  }

  trait QueryMetaHelper[T: Type] extends Extractors {
    import qctx.reflect.report
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

  // Doesn't need to be declared as inline here because case class arguments are automatically inlined that is very cool!
  sealed trait QuotedOperation[T, Op[_]] {
    def op: Quoted[Op[T]]
  }

  // TODO Could make Quoted operation constructor that is a typeclass, not really necessary though
  object QuotedOperation {
    case class QueryOp[T](op: Quoted[Query[T]]) extends QuotedOperation[T, Query]
    case class ActionOp[T](op: Quoted[Action[T]]) extends QuotedOperation[T, Action]
  }


  class RunQuery[
    T: Type, 
    Q[_]: Type,
    ResultRow: Type, 
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quotedOp: Expr[QuotedOperation[T, Q]], 
    ContextOperation: Expr[ContextOperation[T, D, N, PrepareRow, ResultRow, Res]])(using val qctx: Quotes) 
  extends SummonHelper[ResultRow] 
    with QueryMetaHelper[T] 
    with Extractors:
    
    import qctx.reflect._

    enum ExtractBehavior:
      case Extract
      case Skip

    enum QuotationType:
      case Action
      case Query

    /** Run a query with a given QueryMeta given by the output type RawT and the conversion RawT back to T */
    def runWithQueryMeta[RawT: Type](quoted: Expr[Quoted[Query[T]]]): Expr[Res] =
      val (queryRawT, converter, staticStateOpt) = QueryMetaExtractor.applyImpl[T, RawT, D, N](quoted)
      staticStateOpt match {
        case Some(staticState) =>
          executeStatic[RawT](staticState, converter, ExtractBehavior.Extract)
        case None => 
          // NOTE: Can assume QuotationType is `Query` here since summonly a Query-meta is only allowed for Queries
          executeDynamic[RawT, Query](queryRawT, converter, ExtractBehavior.Extract, QuotationType.Query)
      }
    

    /**
     * Expand dynamic-queries i.e. queries whose query-string cannot be computed at compile-time.
     * Note that for now, QuotationType is only needed for dynamic queries (which is only needed to know whether you
     * need to use ElaborateStructure or not. This is decided in the StaticTranslationMacro for static queries using a 
     * different method. I.e. since StaticTranslationMacro knows the AST node it infers Action/Query from that).
     */
    def executeDynamic[RawT: Type, Q[_]: Type](quote: Expr[Quoted[Q[RawT]]], converter: Expr[RawT => T], extract: ExtractBehavior, quotationType: QuotationType) =
      val decoder = summonDecoderOrThrow[RawT]
      // Expand the outermost quote using the macro and put it back into the quote
      // Is the expansion on T or RawT, need to investigate
      val expandedAst = quotationType match
        case QuotationType.Query => ElaborateStructure.ontoDynamicAst[T]('{ $quote.ast })
        case QuotationType.Action => '{ $quote.ast }
      
      val expandedAstQuote = '{ $quote.copy(ast = $expandedAst) }

      // TODO Allow passing in a starting index here?
      // Move this prepare down into RunDynamicExecution since need to use ReifyStatement to know what lifts to call when?
      val extractor = extract match
        case ExtractBehavior.Extract => '{ Some( (r: ResultRow) => $converter.apply($decoder.apply(0, r)) ) }
        case ExtractBehavior.Skip =>    '{ None }

      // TODO What about when an extractor is not neededX
      '{ RunDynamicExecution.apply[RawT, T, Q, D, N, PrepareRow, ResultRow, Res]($expandedAstQuote, $ContextOperation, $extractor) }
    

    
    def resolveLazyLifts(lifts: List[Expr[Planter[?, ?]]]): List[Expr[EagerPlanter[?, ?]]] =
      lifts.map {
        case '{ ($e: EagerPlanter[a, b]) } => e
        case '{ $l: LazyPlanter[a, b] } =>
          val tpe = l.asTerm.tpe
          tpe.asType match {
            case '[LazyPlanter[t, row]] =>
              println(s"Summoning type: ${TypeRepr.of[t].show}")
              Expr.summon[GenericEncoder[t, ResultRow]] match {
                case Some(decoder) =>
                  '{ EagerPlanter[t, ResultRow]($l.value.asInstanceOf[t], $decoder, $l.uid) }
                case None => 
                  report.throwError("Encoder could not be summoned during lazy-lift resolution")
              }
          }
      }

    /** 
     * Execute static query via ctx.executeQuery method given we have the ability to do so 
     * i.e. have a staticState 
     */
    def executeStatic[RawT: Type](staticState: StaticState, converter: Expr[RawT => T], extract: ExtractBehavior): Expr[Res] =    
      val StaticState(query, allLifts) = staticState
      val lifts = Expr.ofList(resolveLazyLifts(allLifts))
      val decoder = summonDecoderOrThrow[RawT]

      // Create the row-preparer to prepare the SQL Query object (e.g. PreparedStatement)
      // and the extractor to read out the results (e.g. ResultSet)
      val prepare = '{ (row: PrepareRow) => LiftsExtractor.apply[PrepareRow]($lifts, row) }
      val extractor = extract match
        // TODO Allow passing in a starting index here?
        case ExtractBehavior.Extract => '{ Some( (r: ResultRow) => $converter.apply($decoder.apply(0, r)) ) }
        case ExtractBehavior.Skip =>    '{ None }
      // Plug in the components and execute
      '{ $ContextOperation.execute(${Expr(query)}, $prepare, $extractor, ExecutionType.Static) }

    // Simple ID function that we use in a couple of places
    private val idConvert = '{ (t:T) => t }

    /** Summon all needed components and run executeQuery method */
    def applyQuery(quoted: Expr[Quoted[Query[T]]]): Expr[Res] =
      summonMetaIfExists match
        // Can we get a QueryMeta? Run that pipeline if we can
        case Some(rowRepr) =>
          rowRepr match { case '[rawT] => runWithQueryMeta[rawT](quoted) } 
        case None =>
          // Otherwise the regular pipeline
          StaticTranslationMacro.applyInner[Query, T, D, N](quoted) match 
            // Can we re-create needed info to construct+tokenize the query statically?
            case Some(staticState) =>
              executeStatic[T](staticState, idConvert, ExtractBehavior.Extract) // Yes we can, do it!
            case None => 
              executeDynamic(quoted, idConvert, ExtractBehavior.Extract, QuotationType.Query)        // No we can't. Do dynamic

    def applyAction(quoted: Expr[Quoted[Action[T]]]): Expr[Res] =
      StaticTranslationMacro.applyInner[Action, T, D, N](quoted) match 
        case Some(staticState) =>
          executeStatic[T](staticState, idConvert, ExtractBehavior.Skip)
        case None => 
          executeDynamic(quoted, idConvert, ExtractBehavior.Skip, QuotationType.Action)

    def apply() =
      quotedOp match
        case '{ QuotedOperation.QueryOp.apply[T]($op) } => applyQuery(op)
        case '{ QuotedOperation.ActionOp.apply[T]($op) } => applyAction(op)
        case _ => report.throwError(s"Could not match the QuotedOperation clause: ${quotedOp.show}")

  end RunQuery


  inline def apply[
    T, 
    Q[_],
    ResultRow, 
    PrepareRow, 
    D <: Idiom, 
    N <: NamingStrategy, 
    Res
  ](inline quotedOp: QuotedOperation[T, Q], ctx: ContextOperation[T, D, N, PrepareRow, ResultRow, Res]) = 
    ${ applyImpl('quotedOp, 'ctx) }
  
  def applyImpl[
    T: Type, 
    Q[_]: Type,
    ResultRow: Type, 
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quotedOp: Expr[QuotedOperation[T, Q]], 
    ctx: Expr[ContextOperation[T, D, N, PrepareRow, ResultRow, Res]])(using qctx: Quotes): Expr[Res] =
    new RunQuery[T, Q, ResultRow, PrepareRow, D, N, Res](quotedOp, ctx).apply()



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
    ctx: ContextOperation[T, D, N, PrepareRow, ResultRow, Res],
    extractor: Option[ResultRow => T]
  ): Res = 
  {
    def gatherLifts(quoted: Quoted[_]): List[Planter[_, _]] =
      quoted.lifts ++ quoted.runtimeQuotes.flatMap(vase => gatherLifts(vase.quoted))

    def spliceQuotations(quoted: Quoted[_]): Ast = {
      val quotationVases = quoted.runtimeQuotes
      val ast = quoted.ast
      // Get all the quotation tags
      Transform(ast) {
        // Splice the corresponding vase for every tag, then recurse
        case v @ QuotationTag(uid) => 
          // When a quotation to splice has been found, retrieve it and continue
          // splicing inside since there could be nested sections that need to be spliced
          quotationVases.find(_.uid == uid) match {
            case Some(vase) => 
              spliceQuotations(vase.quoted)
            case None =>
              throw new IllegalArgumentException(s"Quotation vase with UID ${uid} could not be found!")
          }
      }
    }

    // Splice all quotation values back into the AST recursively, by this point these quotations are dynamic
    // which means that the compiler has not done the splicing for us. We need to do this ourselves. 
    // So we need to go through all the QuotationTags in the AST and splice in the corresponding QuotationVase into it's place.
    val splicedAst = spliceQuotations(quoted)

    // TODO Should make this enable-able via a logging configuration
    //println("=============== Dynamic Expanded Ast Is ===========\n" + io.getquill.util.Messages.qprint(splicedAst))
      
    // Tokenize the spliced AST
    val (outputAst, stmt) = ctx.idiom.translate(splicedAst)(using ctx.naming)

    // TODO check if there are ScalarTag instances that have no lifts and blow them up
    // (shuold test this scenario for both compile-time and dynamic query variants)

    // Turn the Tokenized AST into an actual string and pull out the ScalarTags (i.e. the lifts)
    val (string, externals) =
      ReifyStatement(
        ctx.idiom.liftingPlaceholder,
        ctx.idiom.emptySetContainsToken,
        stmt,
        forProbing = false
      )

    // Get the UIDs from the lifts, if they are something unexpected (e.g. Lift elements from Quill 2.x) throw an exception
    val liftTags = 
      externals.map {
        case ScalarTag(uid) => uid
        case other => throw new IllegalArgumentException(s"Invalid Lift Tag: ${other}")
      }

    // Pull out the all the Planter instances (for now they need to be EagerPlanters for Dynamic Queries)
    val lifts = gatherLifts(quoted).map(lift => (lift.uid, lift)).toMap

    // Match the ScalarTags we pulled out earlier (in ReifyStatement) with corresponding Planters because
    // the Planters can be out of order (I.e. in a different order then the ?s in the SQL query that they need to be spliced into). 
    // The ScalarTags are comming directly from the tokenized AST however and their order should be correct.
    val sortedLifts = liftTags.map { tag =>
      lifts.get(tag) match
        case Some(lift) => lift
        case None => throw new IllegalArgumentException(s"Could not lookup value for the tag: ${tag}")
    }
    // Use the sortedLifts to prepare the method that will prepare the SQL statement
    val prepare = (row: PrepareRow) => LiftsExtractor.withLazy[PrepareRow](sortedLifts, row)

    // Exclute the SQL Statement
    ctx.execute(string, prepare, extractor, ExecutionType.Dynamic)
  }

end RunDynamicExecution