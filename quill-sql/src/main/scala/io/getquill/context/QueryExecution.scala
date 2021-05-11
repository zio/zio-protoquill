package io.getquill.context

import io.getquill.norm.BetaReduction
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
import io.getquill.generic.DecodingType
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
import io.getquill.ActionReturning
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.metaprog.Extractors
import io.getquill.BatchAction
import io.getquill._
import io.getquill.parser.Lifter

trait ContextOperation[I, T, D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Res](val idiom: D, val naming: N) {
  def execute(sql: String, prepare: PrepareRow => (List[Any], PrepareRow), extractor: Extraction[ResultRow, T], executionInfo: ExecutionInfo): Res
}

/**
 * Drives execution of Quoted blocks i.e. Queries etc... from the context.
 */
object QueryExecution:

  trait SummonHelper[ResultRow: Type] {
    implicit val qctx: Quotes
    import qctx.reflect._

    /** Summon decoder for a given Type and Row type (ResultRow) */
    def summonDecoderOrThrow[DecoderT: Type](): Expr[GenericDecoder[ResultRow, DecoderT, DecodingType]] = {
      // First try summoning a specific encoder, if that doesn't work, summon the generic one
      Expr.summon[GenericDecoder[ResultRow, DecoderT, DecodingType.Specific]] match
        case Some(decoder) => decoder
        case None =>
          Expr.summon[GenericDecoder[ResultRow, DecoderT, DecodingType.Generic]] match
            case Some(decoder) => decoder
            case None => report.throwError(s"Decoder could not be summoned during query execution for the type ${io.getquill.util.Format.TypeOf[DecoderT]}")
    }
  }

  trait QueryMetaHelper[T: Type] extends Extractors {
    import qctx.reflect.report
    // See if there there is a QueryMeta mapping T to some other type RawT
    def summonQueryMetaIfExists() =
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

  enum ExtractBehavior:
      case Extract
      case ExtractWithReturnAction
      case Skip

  enum ElaborationBehavior:
      case Elaborate
      case Skip

  class RunQuery[
    I: Type, 
    T: Type,
    ResultRow: Type, 
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quotedOp: Expr[Quoted[QAC[I, T]]], 
    contextOperation: Expr[ContextOperation[I, T, D, N, PrepareRow, ResultRow, Res]])(using val qctx: Quotes, QAC: Type[QAC[I, T]]) 
  extends SummonHelper[ResultRow] 
    with QueryMetaHelper[T] 
    with Extractors:
    
    import qctx.reflect._

    def apply() =
      QAC match
        case '[QAC[Nothing, T]] => applyQuery(quotedOp)
        case '[QAC[I, Nothing]] => applyAction(quotedOp)
        case '[QAC[I, T]] => 
          if (!(TypeRepr.of[T] =:= TypeRepr.of[Any]))
            applyActionReturning(quotedOp) // ReturningAction is also a subtype of Action so check it before Action
          else
            // In certain situations (i.e. if a user does infix"stuff".as[Actoin[Stuff]] something will be directly specified
            // as an Action[T] without there being a `& QAC[T, Nothing]` as part of the type. In that case, the `ModificationEntity`
            // will just be `Any`. We need to manually detect that case since it requires no return type)
            applyAction(quotedOp)
        case _ => 
          report.throwError(s"Could not match type type of the quoted operation: ${io.getquill.util.Format.Type(QAC)}")

        // Simple ID function that we use in a couple of places
    private val idConvert = '{ (t:T) => t }

    /** Summon all needed components and run executeQuery method */
    def applyQuery(quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      summonQueryMetaIfExists() match
        // Can we get a QueryMeta? Run that pipeline if we can
        case Some(queryMeta) =>
          queryMeta match { case '[rawT] => runWithQueryMeta[rawT](quoted) } 
        case None =>
          // Otherwise the regular pipeline
          StaticTranslationMacro.applyInner[I, T, D, N](quoted) match 
            // Can we re-create needed info to construct+tokenize the query statically?
            case Some(staticState) =>
              executeStatic[T](staticState, idConvert, ExtractBehavior.Extract) // Yes we can, do it!
            case None => 
              executeDynamic(quoted, idConvert, ExtractBehavior.Extract, ElaborationBehavior.Elaborate) // No we can't. Do dynamic

    def applyAction(quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      StaticTranslationMacro.applyInner[I, T, D, N](quoted) match 
        case Some(staticState) =>
          executeStatic[T](staticState, idConvert, ExtractBehavior.Skip)
        case None => 
          executeDynamic(quoted, idConvert, ExtractBehavior.Skip, ElaborationBehavior.Skip)

    def applyActionReturning(quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      StaticTranslationMacro.applyInner[I, T, D, N](quoted) match 
        case Some(staticState) =>
          executeStatic[T](staticState, idConvert, ExtractBehavior.ExtractWithReturnAction)
        case None => 
          executeDynamic(quoted, idConvert, ExtractBehavior.ExtractWithReturnAction, ElaborationBehavior.Skip)

    /** Run a query with a given QueryMeta given by the output type RawT and the conversion RawT back to T */
    def runWithQueryMeta[RawT: Type](quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      val (queryRawT, converter, staticStateOpt) = QueryMetaExtractor.applyImpl[T, RawT, D, N](quoted.asExprOf[Quoted[Query[T]]])
      staticStateOpt match {
        case Some(staticState) =>
          executeStatic[RawT](staticState, converter, ExtractBehavior.Extract)
        case None => 
          // NOTE: Can assume QuotationType is `Query` here since summonly a Query-meta is only allowed for Queries
          // TODO refacotry QueryMetaExtractor to use QAC[I, T] => QAC[I, RawT] then use that
          executeDynamic[RawT](queryRawT.asExprOf[Quoted[QAC[I, RawT]]], converter, ExtractBehavior.Extract, ElaborationBehavior.Elaborate)
      }
    
    def resolveLazyLifts(lifts: List[Expr[Planter[?, ?]]]): List[Expr[Planter[?, ?]]] =
      lifts.map {
        case '{ ($e: EagerPlanter[a, b]) } => e
        case '{ ($e: EagerListPlanter[a, b]) } => e
        case '{ $l: LazyPlanter[a, b] } =>
          val tpe = l.asTerm.tpe
          tpe.asType match
            case '[LazyPlanter[t, row]] =>
              println(s"Summoning type: ${TypeRepr.of[t].show}")
              Expr.summon[GenericEncoder[t, ResultRow]] match
                case Some(decoder) =>
                  '{ EagerPlanter[t, ResultRow]($l.value.asInstanceOf[t], $decoder, $l.uid) }
                case None => 
                  report.throwError("Encoder could not be summoned during lazy-lift resolution")
        case other =>
          report.throwError(s"""|
            |Invalid planter found during lazy lift resolution:
            |${io.getquill.util.Format.Expr(other)} 
            |All injectable planters should already have been elaborated into separate components.
            """.stripMargin)
      }

    def makeDecoder[RawT: Type]() = summonDecoderOrThrow[RawT]()
    def makeExtractorFrom[RawT: Type](contramap: Expr[RawT => T]) =
      val decoder = makeDecoder()
      '{ (r: ResultRow) => $contramap.apply(${decoder}.apply(0, r)) }

    /** 
     * Execute static query via ctx.executeQuery method given we have the ability to do so 
     * i.e. have a staticState 
     */
    def executeStatic[RawT: Type](state: StaticState, converter: Expr[RawT => T], extract: ExtractBehavior): Expr[Res] =
      val lifts = resolveLazyLifts(state.lifts)
      
      // Create the row-preparer to prepare the SQL Query object (e.g. PreparedStatement)
      // and the extractor to read out the results (e.g. ResultSet)
      val prepare = '{ (row: PrepareRow) => LiftsExtractor.apply[PrepareRow](${Expr.ofList(lifts)}, row) }
      val extractor: Expr[io.getquill.context.Extraction[ResultRow, T]] = 
        extract match
          // TODO Allow passing in a starting index here?
          case ExtractBehavior.Extract => 
            val extractor = makeExtractorFrom[RawT](converter)
            '{ Extraction.Simple($extractor) }
          case ExtractBehavior.ExtractWithReturnAction =>
            val extractor = makeExtractorFrom[RawT](converter)
            val returnAction = state.returnAction.getOrElse { throw new IllegalArgumentException(s"Return action could not be found in the Query: ${query}") }
            '{ Extraction.Returning($extractor, ${io.getquill.parser.Lifter.returnAction(returnAction)}) }
          case ExtractBehavior.Skip =>    
            '{ Extraction.None }

      val particularQuery = Particularize.Static(state.query, lifts, '{ $contextOperation.idiom.liftingPlaceholder })
      // Plug in the components and execute
      '{ $contextOperation.execute($particularQuery, $prepare, $extractor, ExecutionInfo(ExecutionType.Static, ${Lifter(state.ast)})) }
    end executeStatic

    /**
     * Expand dynamic-queries i.e. queries whose query-string cannot be computed at compile-time.
     * Note that for now, QuotationType is only needed for dynamic queries (which is only needed to know whether you
     * need to use ElaborateStructure or not. This is decided in the StaticTranslationMacro for static queries using a 
     * different method. I.e. since StaticTranslationMacro knows the AST node it infers Action/Query from that).
     */
    def executeDynamic[RawT: Type](quote: Expr[Quoted[QAC[I, RawT]]], converter: Expr[RawT => T], extract: ExtractBehavior, quotationType: ElaborationBehavior) =
      // Expand the outermost quote using the macro and put it back into the quote
      // Is the expansion on T or RawT, need to investigate
      // Note that in the Static case, this is done by checking the Type of the root entity. If the root Entity
      // is a Query, then the Elaboration happens. For the dynamic query variation, this is more difficult to do because
      // we have Expr[Ast] instead of Ast in the times when we have Type[T]. We could Elaborate T during compile-time
      // and then decide to plug in the elaboration or not during runtime (depending on the type of Ast which would be runtime-checked)
      // but that would be less straightforward to do.
      val elaboratedAst = quotationType match
        case ElaborationBehavior.Elaborate => ElaborateStructure.ontoDynamicAst[T]('{ $quote.ast })
        case ElaborationBehavior.Skip => '{ $quote.ast }
      
      val elaboratedAstQuote = '{ $quote.copy(ast = $elaboratedAst) }

      // TODO Allow passing in a starting index here?
      // Move this prepare down into RunDynamicExecution since need to use ReifyStatement to know what lifts to call when?
      val extractor: Expr[io.getquill.context.Extraction[ResultRow, T]] = 
        extract match
          case ExtractBehavior.Extract =>
            val extractor = makeExtractorFrom[RawT](converter)
            '{ Extraction.Simple( $extractor ) }
          // if a return action is needed, that ReturnAction will be calculated later in the dynamic context
          // therefore here, all we do is to pass in a extractor. We will compute the Extraction.ReturningLater
          case ExtractBehavior.ExtractWithReturnAction =>
            val extractor = makeExtractorFrom[RawT](converter)
            '{ Extraction.Simple( $extractor ) }
          case ExtractBehavior.Skip =>    
            '{ Extraction.None }

      // TODO What about when an extractor is not neededX
      '{ RunDynamicExecution.apply[I, T, RawT, D, N, PrepareRow, ResultRow, Res]($elaboratedAstQuote, $contextOperation, $extractor) }
    end executeDynamic

  end RunQuery


  inline def apply[
    I,
    T, 
    ResultRow, 
    PrepareRow, 
    D <: Idiom, 
    N <: NamingStrategy, 
    Res
  ](inline quotedOp: Quoted[QAC[I, T]], ctx: ContextOperation[I, T, D, N, PrepareRow, ResultRow, Res]) = 
    ${ applyImpl('quotedOp, 'ctx) }
  
  def applyImpl[
    I: Type,
    T: Type, 
    ResultRow: Type, 
    PrepareRow: Type, 
    D <: Idiom: Type, 
    N <: NamingStrategy: Type, 
    Res: Type
  ](quotedOp: Expr[Quoted[QAC[I, T]]], 
    ctx: Expr[ContextOperation[I, T, D, N, PrepareRow, ResultRow, Res]])(using qctx: Quotes): Expr[Res] =
    new RunQuery[I, T, ResultRow, PrepareRow, D, N, Res](quotedOp, ctx).apply()



end QueryExecution

/**
 * Drives dynamic execution from the Context
 * Note that AST is already elaborated by the time it comes into here
 */
object RunDynamicExecution:

  import io.getquill.idiom.{ Idiom => Idiom }
  import io.getquill.{ NamingStrategy => NamingStrategy }
  import io.getquill.idiom.Statement
  import io.getquill.ast.ReturningAction

  def apply[
    I,
    T,
    RawT, 
    D <: Idiom, 
    N <: NamingStrategy, 
    PrepareRow, 
    ResultRow, 
    Res
  ](quoted: Quoted[QAC[I, RawT]], 
    ctx: ContextOperation[I, T, D, N, PrepareRow, ResultRow, Res],
    rawExtractor: Extraction[ResultRow, T]
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
    // (also, we need to tell if ReturningGenerated is the top-level element in order to know that the
    // extraction type is Extraction.Returning by in some cases the AST will be 
    // FunctionApply(Function(ident, ReturningGenerated(...))), stuff). In those cases, we need
    // to do a beta-reduction first.
    val splicedAst = BetaReduction(spliceQuotations(quoted))

    // TODO Should make this enable-able via a logging configuration
    //println("=============== Dynamic Expanded Ast Is ===========\n" + io.getquill.util.Messages.qprint(splicedAst))
      
    // Tokenize the spliced AST
    val (outputAst, stmt) = ctx.idiom.translate(splicedAst)(using ctx.naming)
    val naiveQury = Unparticular.translateNaive(stmt, ctx.idiom.liftingPlaceholder)

    val liftColumns =
      (ast: Ast, stmt: Statement) => Unparticular.translateNaive(stmt, ctx.idiom.liftingPlaceholder)

    val returningActionOpt = splicedAst match
      // If we have a returning action, we need to compute some additional information about how to return things.
      // Different database dialects handle these things differently. Some allow specifying a list of column-names to
      // return from the query. Others compute this information from the query data directly. This information is stored
      // in the dialect and therefore is computed here.
      case returningActionAst: ReturningAction =>
        Some(io.getquill.norm.ExpandReturning.applyMap(returningActionAst)(liftColumns)(ctx.idiom, ctx.naming))
      case _ =>
        None

    val extractor = (rawExtractor, returningActionOpt) match
      case (Extraction.Simple(extract), Some(returningAction)) => Extraction.Returning(extract, returningAction)
      case (Extraction.Simple(_), None) => rawExtractor
      case (Extraction.None, None) => rawExtractor
      case (extractor, returningAction) => throw new IllegalArgumentException(s"Invalid state. Cannot have ${extractor} with a returning action ${returningAction}")

    // Turn the Tokenized AST into an actual string and pull out the ScalarTags (i.e. the lifts)
    val (unparticularQuery, externals) = Unparticular.Query.fromStatement(stmt, ctx.idiom.liftingPlaceholder)

    // Get the UIDs from the lifts, if they are something unexpected (e.g. Lift elements from Quill 2.x) throw an exception
    val liftTags = 
      externals.map {
        case ScalarTag(uid) => uid
        case other => throw new IllegalArgumentException(s"Invalid Lift Tag: ${other}")
      }

    // Pull out the all the Planter instances (for now they need to be EagerPlanters for Dynamic Queries)
    val gatheredLifts = gatherLifts(quoted)
    val lifts = gatheredLifts.map(lift => (lift.uid, lift)).toMap

    val queryString = Particularize.Dynamic(unparticularQuery, gatheredLifts, ctx.idiom.liftingPlaceholder)

    // Match the ScalarTags we pulled out earlier (in ReifyStatement) with corresponding Planters because
    // the Planters can be out of order (I.e. in a different order then the ?s in the SQL query that they need to be spliced into). 
    // The ScalarTags are comming directly from the tokenized AST however and their order should be correct.
    val sortedLifts = liftTags.map { tag =>
      lifts.get(tag) match
        case Some(lift) => lift
        case None => throw new IllegalArgumentException(s"Could not lookup value for the tag: ${tag}")
    }
    // Use the sortedLifts to prepare the method that will prepare the SQL statement
    val prepare = (row: PrepareRow) => LiftsExtractor.Dynamic[PrepareRow](sortedLifts, row)

    // Exclute the SQL Statement
    ctx.execute(queryString, prepare, extractor, ExecutionInfo(ExecutionType.Dynamic, outputAst))
  }

end RunDynamicExecution