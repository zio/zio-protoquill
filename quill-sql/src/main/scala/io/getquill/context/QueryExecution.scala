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
import io.getquill.metaprog.Extractors._
import io.getquill.BatchAction
import io.getquill._
import io.getquill.parser.Lifter
import io.getquill.OuterSelectWrap
import io.getquill.util.CommonExtensions

trait ContextOperation[I, T, D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Session, Ctx <: Context[_, _], Res](val idiom: D, val naming: N) {
  def execute(sql: String, prepare: (PrepareRow, Session) => (List[Any], PrepareRow), extractor: Extraction[ResultRow, Session, T], executionInfo: ExecutionInfo, fetchSize: Option[Int]): Res
}

/** Enums and helper methods for QueryExecution and BatchQueryExecution */
object Execution:

  enum ExtractBehavior:
    case Extract
    case ExtractWithReturnAction
    case Skip

  enum ElaborationBehavior:
    case Elaborate
    case Skip

  // Simple ID function that we use in a couple of places
  def identityConverter[T: Type](using Quotes) = '{ (t:T) => t }

  /** Summon decoder for a given Type and Row type (ResultRow) */
  def summonDecoderOrThrow[ResultRow: Type, Session: Type, DecoderT: Type]()(using Quotes): Expr[GenericDecoder[ResultRow, Session, DecoderT, DecodingType]] =
    import quotes.reflect.{ Try => _, _}
    // First try summoning a specific encoder, if that doesn't work, summon the generic one
    Expr.summon[GenericDecoder[ResultRow, Session, DecoderT, DecodingType.Specific]] match
      case Some(decoder) => decoder
      case None =>
        Expr.summon[GenericDecoder[ResultRow, Session, DecoderT, DecodingType.Generic]] match
          case Some(decoder) => decoder
          case None =>
            println(s"Error summoning Decoder: could not be summoned during query execution for the type ${io.getquill.util.Format.TypeOf[DecoderT]}")
            report.throwError(s"Decoder could not be summoned during query execution for the type ${io.getquill.util.Format.TypeOf[DecoderT]}")

  /** See if there there is a QueryMeta mapping T to some other type RawT */
  def summonQueryMetaTypeIfExists[T: Type](using Quotes) =
    import quotes.reflect._
    Expr.summon[QueryMeta[T, _]] match
      case Some(expr) =>
        expr.asTerm.tpe.asType match
          case '[QueryMeta[k, n]] => Some(Type.of[n])
      case None => None

  def makeDecoder[ResultRow: Type, Session: Type, RawT: Type](using Quotes)() = summonDecoderOrThrow[ResultRow, Session, RawT]()

  class MakeExtractor[ResultRow: Type, Session: Type, T: Type, RawT: Type]:
    def makeExtractorFrom(contramap: Expr[RawT => T])(using Quotes) =
      val decoder = makeDecoder[ResultRow, Session, RawT]()
      '{ (r: ResultRow, s: Session) => $contramap.apply(${decoder}.apply(0, r, s)) }

    def static(state: StaticState, converter: Expr[RawT => T], extract: ExtractBehavior)(using Quotes): Expr[io.getquill.context.Extraction[ResultRow, Session, T]] =
      extract match
        // TODO Allow passing in a starting index here?
        case ExtractBehavior.Extract =>
          val extractor = makeExtractorFrom(converter)
          '{ Extraction.Simple($extractor) }
        case ExtractBehavior.ExtractWithReturnAction =>
          val extractor = makeExtractorFrom(converter)
          val returnAction = state.returnAction.getOrElse { throw new IllegalArgumentException(s"Return action could not be found in the Query: ${query}") }
          '{ Extraction.Returning($extractor, ${io.getquill.parser.Lifter.returnAction(returnAction)}) }
        case ExtractBehavior.Skip =>
          '{ Extraction.None }

    def dynamic(converter: Expr[RawT => T], extract: ExtractBehavior)(using Quotes): Expr[io.getquill.context.Extraction[ResultRow, Session, T]] =
      extract match
        case ExtractBehavior.Extract =>
          val extractor = makeExtractorFrom(converter)
          '{ Extraction.Simple( $extractor ) }
        // if a return action is needed, that ReturnAction will be calculated later in the dynamic context
        // therefore here, all we do is to pass in a extractor. We will compute the Extraction.ReturningLater
        case ExtractBehavior.ExtractWithReturnAction =>
          val extractor = makeExtractorFrom(converter)
          '{ Extraction.Simple( $extractor ) }
        case ExtractBehavior.Skip =>
          '{ Extraction.None }


  end MakeExtractor

end Execution

/**
 * Drives execution of Quoted blocks i.e. Queries etc... from the context.
 */
object QueryExecution:



  class RunQuery[
    I: Type,
    T: Type,
    ResultRow: Type,
    PrepareRow: Type,
    Session: Type,
    D <: Idiom: Type,
    N <: NamingStrategy: Type,
    Ctx <: Context[_, _]: Type,
    Res: Type
  ](
    quotedOp: Expr[Quoted[QAC[I, T]]],
    contextOperation: Expr[ContextOperation[I, T, D, N, PrepareRow, ResultRow, Session, Ctx, Res]],
    fetchSize: Expr[Option[Int]],
    wrap: Expr[OuterSelectWrap]
  )(using val qctx: Quotes, QAC: Type[QAC[I, T]]):
    import qctx.reflect.{ Try => _, _ }
    import Execution._

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

    lazy val wrapValue = OuterSelectWrap.unlift(wrap)
    lazy val queryElaborationBehavior =
      wrapValue match
        case OuterSelectWrap.Always => ElaborationBehavior.Elaborate
        case OuterSelectWrap.Never => ElaborationBehavior.Skip
        case OuterSelectWrap.Default => ElaborationBehavior.Elaborate

    /**
     * Summon all needed components and run executeQuery method
     * (Experiment with catching `StaticTranslationMacro.apply` errors since they usually happen
     * because some upstream construct has done a reportError so we do not want to do another one.
     * I.e. if we do another returnError here it will override that one which is not needed.
     * if this seems to work well, make the same change to other apply___ methods here.
     * )
     */
    def applyQuery(quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      summonQueryMetaTypeIfExists[T] match
        // Can we get a QueryMeta? Run that pipeline if we can
        case Some(queryMeta) =>
          queryMeta match { case '[rawT] => runWithQueryMeta[rawT](quoted) }
        case None =>
          Try(StaticTranslationMacro[I, T, D, N](quoted, queryElaborationBehavior)) match
            case scala.util.Failure(e) =>
              import CommonExtensions.Throwable._
              val msg = s"Query splicing failed due to error: ${e.stackTraceToString}"
              // TODO When a trace logger is found instrument this
              // println(s"[InternalError] ${msg}")
              // Return a throw if static translation failed. This typically results from a higher-level returnError that has already returned
              // if we do another returnError here it will override that one which is not needed.
              report.throwError(msg)
            // Otherwise the regular pipeline
            case scala.util.Success(Some(staticState)) =>
              executeStatic[T](staticState, identityConverter, ExtractBehavior.Extract) // Yes we can, do it!
            case scala.util.Success(None) =>
              executeDynamic(quoted, identityConverter, ExtractBehavior.Extract, queryElaborationBehavior) // No we can't. Do dynamic

    def applyAction(quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      StaticTranslationMacro[I, T, D, N](quoted, ElaborationBehavior.Skip) match
        case Some(staticState) =>
          executeStatic[T](staticState, identityConverter, ExtractBehavior.Skip)
        case None =>
          executeDynamic(quoted, identityConverter, ExtractBehavior.Skip, ElaborationBehavior.Skip)

    def applyActionReturning(quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      StaticTranslationMacro[I, T, D, N](quoted, ElaborationBehavior.Skip) match
        case Some(staticState) =>
          executeStatic[T](staticState, identityConverter, ExtractBehavior.ExtractWithReturnAction)
        case None =>
          executeDynamic(quoted, identityConverter, ExtractBehavior.ExtractWithReturnAction, ElaborationBehavior.Skip)

    /** Run a query with a given QueryMeta given by the output type RawT and the conversion RawT back to T */
    def runWithQueryMeta[RawT: Type](quoted: Expr[Quoted[QAC[I, T]]]): Expr[Res] =
      val (queryRawT, converter, staticStateOpt) = QueryMetaExtractor.applyImpl[T, RawT, D, N](quoted.asExprOf[Quoted[Query[T]]])
      staticStateOpt match {
        case Some(staticState) =>
          executeStatic[RawT](staticState, converter, ExtractBehavior.Extract)
        case None =>
          // NOTE: Can assume QuotationType is `Query` here since summonly a Query-meta is only allowed for Queries
          // TODO refacotry QueryMetaExtractor to use QAC[I, T] => QAC[I, RawT] then use that
          executeDynamic[RawT](queryRawT.asExprOf[Quoted[QAC[I, RawT]]], converter, ExtractBehavior.Extract, queryElaborationBehavior)
      }

    def resolveLazyLiftsStatic(lifts: List[Expr[Planter[?, ?, ?]]]): List[Expr[Planter[?, ?, ?]]] =
      import io.getquill.metaprog.{ LazyPlanterExpr, EagerPlanterExpr }
      lifts.map {
        case '{ ($e: EagerPlanter[a, b, c]) } => e
        case '{ ($e: EagerListPlanter[a, b, c]) } => e
        case l @ PlanterExpr.Uprootable(expr @ LazyPlanterExpr(uid, value)) =>
          val tpe = l.asTerm.tpe.widen
          tpe.asType match
            case '[LazyPlanter[t, row, session]] =>
              Expr.summon[GenericEncoder[t, ResultRow, Session]] match
                case Some(decoder) =>
                  EagerPlanterExpr(uid, value.asInstanceOf[Expr[t]], decoder).plant
                case None =>
                  report.throwError("Encoder could not be summoned during lazy-lift resolution")
        case other =>
          report.throwError(s"""|
            |Invalid planter found during lazy lift resolution:
            |${io.getquill.util.Format.Expr(other)}
            |All injectable planters should already have been elaborated into separate components.
            """.stripMargin)
      }

    /**
     * Execute static query via ctx.executeQuery method given we have the ability to do so
     * i.e. have a staticState
     */
    def executeStatic[RawT: Type](state: StaticState, converter: Expr[RawT => T], extract: ExtractBehavior): Expr[Res] =
      val lifts = resolveLazyLiftsStatic(state.lifts)

      // Create the row-preparer to prepare the SQL Query object (e.g. PreparedStatement)
      // and the extractor to read out the results (e.g. ResultSet)
      val prepare = '{ (row: PrepareRow, session: Session) => LiftsExtractor.apply[PrepareRow, Session](${Expr.ofList(lifts)}, row, session) }
      val extractor = MakeExtractor[ResultRow, Session, T, RawT].static(state, converter, extract)

      val particularQuery = Particularize.Static(state.query, lifts, '{ $contextOperation.idiom.liftingPlaceholder }, state.idiom.emptySetContainsToken)
      // Plug in the components and execute
      val astSplice =
        if (TypeRepr.of[Ctx] <:< TypeRepr.of[AstSplicing]) Lifter(state.ast)
        else '{ io.getquill.ast.NullValue }
      '{ $contextOperation.execute($particularQuery, $prepare, $extractor, ExecutionInfo(ExecutionType.Static, $astSplice), $fetchSize) }
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
        case ElaborationBehavior.Elaborate => ElaborateStructure.ontoDynamicAst[RawT]('{ $quote.ast })
        case ElaborationBehavior.Skip => '{ $quote.ast }

      val elaboratedAstQuote = '{ $quote.copy(ast = $elaboratedAst) }
      val extractor: Expr[io.getquill.context.Extraction[ResultRow, Session, T]] = MakeExtractor[ResultRow, Session, T, RawT].dynamic(converter, extract)

      // TODO What about when an extractor is not neededX
      val spliceAsts = TypeRepr.of[Ctx] <:< TypeRepr.of[AstSplicing]
      '{ RunDynamicExecution.apply[I, T, RawT, D, N, PrepareRow, ResultRow, Session, Ctx, Res]($elaboratedAstQuote, $contextOperation, $extractor, ${Expr(spliceAsts)}, $fetchSize) }
    end executeDynamic

  end RunQuery


  inline def apply[
    I,
    T,
    ResultRow,
    PrepareRow,
    Session,
    D <: Idiom,
    N <: NamingStrategy,
    Ctx <: Context[_, _],
    Res
  ](inline quotedOp: Quoted[QAC[I, T]], ctx: ContextOperation[I, T, D, N, PrepareRow, ResultRow, Session, Ctx, Res], fetchSize: Option[Int], inline wrap: OuterSelectWrap = OuterSelectWrap.Default) =
    ${ applyImpl('quotedOp, 'ctx, 'fetchSize, 'wrap) }

  def applyImpl[
    I: Type,
    T: Type,
    ResultRow: Type,
    PrepareRow: Type,
    Session: Type,
    D <: Idiom: Type,
    N <: NamingStrategy: Type,
    Ctx <: Context[_, _]: Type,
    Res: Type
  ](
    quotedOp: Expr[Quoted[QAC[I, T]]],
    ctx: Expr[ContextOperation[I, T, D, N, PrepareRow, ResultRow, Session, Ctx, Res]],
    fetchSize: Expr[Option[Int]],
    wrap: Expr[OuterSelectWrap]
  )(using qctx: Quotes): Expr[Res] = new RunQuery[I, T, ResultRow, PrepareRow, Session, D, N, Ctx, Res](quotedOp, ctx, fetchSize, wrap).apply()



end QueryExecution

object PrepareDynamicExecution:
  import io.getquill.idiom.{ Idiom => Idiom }
  import io.getquill.{ NamingStrategy => NamingStrategy }
  import io.getquill.idiom.Statement
  import io.getquill.ast.ReturningAction

  def spliceQuotations(quoted: Quoted[_]): Ast =
    def spliceQuotationsRecurse(quoted: Quoted[_]): Ast =
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
                spliceQuotationsRecurse(vase.quoted)
              case None =>
                throw new IllegalArgumentException(s"Quotation vase with UID ${uid} could not be found!")
            }
        }
    BetaReduction(spliceQuotationsRecurse(quoted))
  end spliceQuotations

  def gatherLifts(quoted: Quoted[_]): List[Planter[_, _, _]] =
    quoted.lifts ++ quoted.runtimeQuotes.flatMap(vase => gatherLifts(vase.quoted))

  enum SpliceBehavior:
    case NeedsSplice
    case AlreadySpliced

  def apply[
    I,
    T,
    RawT,
    D <: Idiom,
    N <: NamingStrategy,
    PrepareRow,
    ResultRow,
    Session
  ](
    quoted: Quoted[QAC[I, RawT]],
    rawExtractor: Extraction[ResultRow, Session, T],
    idiom: D,
    naming: N,
    spliceBehavior: SpliceBehavior = SpliceBehavior.NeedsSplice
  ) =
    // Splice all quotation values back into the AST recursively, by this point these quotations are dynamic
    // which means that the compiler has not done the splicing for us. We need to do this ourselves.
    // So we need to go through all the QuotationTags in the AST and splice in the corresponding QuotationVase into it's place.
    // (also, we need to tell if ReturningGenerated is the top-level element in order to know that the
    // extraction type is Extraction.Returning by in some cases the AST will be
    // FunctionApply(Function(ident, ReturningGenerated(...))), stuff). In those cases, we need
    // to do a beta-reduction first.
    val (splicedAst, gatheredLifts) =
      spliceBehavior match
        case SpliceBehavior.NeedsSplice => (spliceQuotations(quoted), gatherLifts(quoted))
        case SpliceBehavior.AlreadySpliced => (quoted.ast, quoted.lifts) // If already spliced, can skip all runtimeQuotes clauses since their asts have already been spliced, same with lifts

    VerifyFreeVariables.runtime(splicedAst)

    // Pull out the all the Planter instances (for now they need to be EagerPlanters for Dynamic Queries)
    val lifts = gatheredLifts.map(lift => (lift.uid, lift)).toMap

    // TODO Should make this enable-able via a logging configuration
    //println("=============== Dynamic Expanded Ast Is ===========\n" + io.getquill.util.Messages.qprint(splicedAst))

    // Tokenize the spliced AST
    val (outputAst, stmt) = idiom.translate(splicedAst)(using naming)
    val naiveQury = Unparticular.translateNaive(stmt, idiom.liftingPlaceholder)

    val liftColumns =
      (ast: Ast, stmt: Statement) => Unparticular.translateNaive(stmt, idiom.liftingPlaceholder)

    val returningActionOpt = splicedAst match
      // If we have a returning action, we need to compute some additional information about how to return things.
      // Different database dialects handle these things differently. Some allow specifying a list of column-names to
      // return from the query. Others compute this information from the query data directly. This information is stored
      // in the dialect and therefore is computed here.
      case returningActionAst: ReturningAction =>
        Some(io.getquill.norm.ExpandReturning.applyMap(returningActionAst)(liftColumns)(idiom, naming))
      case _ =>
        None

    val extractor = (rawExtractor, returningActionOpt) match
      case (Extraction.Simple(extract), Some(returningAction)) => Extraction.Returning(extract, returningAction)
      case (Extraction.Simple(_), None) => rawExtractor
      case (Extraction.None, None) => rawExtractor
      case (extractor, returningAction) => throw new IllegalArgumentException(s"Invalid state. Cannot have ${extractor} with a returning action ${returningAction}")

    // Turn the Tokenized AST into an actual string and pull out the ScalarTags (i.e. the lifts)
    val (unparticularQuery, externals) = Unparticular.Query.fromStatement(stmt, idiom.liftingPlaceholder)

    // Get the UIDs from the lifts, if they are something unexpected (e.g. Lift elements from Quill 2.x) throw an exception
    val liftTags =
      externals.map {
        case ScalarTag(uid) => uid
        case other => throw new IllegalArgumentException(s"Invalid Lift Tag: ${other}")
      }

    val queryString = Particularize.Dynamic(unparticularQuery, gatheredLifts, idiom.liftingPlaceholder, idiom.emptySetContainsToken)

    // Match the ScalarTags we pulled out earlier (in ReifyStatement) with corresponding Planters because
    // the Planters can be out of order (I.e. in a different order then the ?s in the SQL query that they need to be spliced into).
    // The ScalarTags are comming directly from the tokenized AST however and their order should be correct.
    // also, some of they may be filtered out
    val sortedLifts = liftTags.map { tag =>
      lifts.get(tag) match
        case Some(lift) => lift
        case None => throw new IllegalArgumentException(s"Could not lookup value for the tag: ${tag}")
    }

    (queryString, outputAst, sortedLifts, extractor)

  end apply
end PrepareDynamicExecution

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
    Session,
    Ctx <: Context[_, _],
    Res
  ](quoted: Quoted[QAC[I, RawT]],
    ctx: ContextOperation[I, T, D, N, PrepareRow, ResultRow, Session, Ctx, Res],
    rawExtractor: Extraction[ResultRow, Session, T],
    spliceAst: Boolean,
    fetchSize: Option[Int]
  ): Res =
  {
    //println("===== Passed Ast: " + io.getquill.util.Messages.qprint(quoted.ast))
    val (queryString, outputAst, sortedLifts, extractor) =
      PrepareDynamicExecution[I, T, RawT, D, N, PrepareRow, ResultRow, Session](quoted, rawExtractor, ctx.idiom, ctx.naming)

    // Use the sortedLifts to prepare the method that will prepare the SQL statement
    val prepare = (row: PrepareRow, session: Session) => LiftsExtractor.Dynamic[PrepareRow, Session](sortedLifts, row, session)

    // Exclute the SQL Statement
    val executionAst = if (spliceAst) outputAst else io.getquill.ast.NullValue
    ctx.execute(queryString, prepare, extractor, ExecutionInfo(ExecutionType.Dynamic, executionAst), fetchSize)
  }

end RunDynamicExecution