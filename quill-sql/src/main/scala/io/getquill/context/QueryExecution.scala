package io.getquill.context

import io.getquill.norm.BetaReduction
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
import io.getquill.generic.ElaborateTrivial
import io.getquill.quat.QuatMaking
import io.getquill.quat.Quat
import io.getquill.ast.External
import io.getquill.norm.TranspileConfig
import io.getquill.metaprog.TranspileConfigLiftable
import io.getquill.metaprog.SummonTranspileConfig
import io.getquill.idiom.Token
import io.getquill.util.Interpolator
import io.getquill.util.Messages.TraceType

object ContextOperation:
  case class SingleArgument[I, T, A <: QAC[I, _] with Action[I], D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Session, Ctx <: Context[_, _], Res](
      sql: String,
      prepare: (PrepareRow, Session) => (List[Any], PrepareRow),
      extractor: Extraction[ResultRow, Session, T],
      executionInfo: ExecutionInfo,
      fetchSize: Option[Int]
  )
  case class BatchArgument[I, T, A <: QAC[I, _] with Action[I], D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Session, Ctx <: Context[_, _], Res](
      groups: List[(String, List[(PrepareRow, Session) => (List[Any], PrepareRow)])],
      extractor: Extraction[ResultRow, Session, T],
      executionInfo: ExecutionInfo,
      fetchSize: Option[Int]
  )
  case class Single[I, T, A <: QAC[I, _] with Action[I], D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Session, Ctx <: Context[_, _], Res](val idiom: D, val naming: N)(
      val execute: (ContextOperation.SingleArgument[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res]) => Res
  )
  case class Batch[I, T, A <: QAC[I, _] with Action[I], D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Session, Ctx <: Context[_, _], Res](val idiom: D, val naming: N)(
      val execute: (ContextOperation.BatchArgument[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res]) => Res
  )
  case class Factory[D <: Idiom, N <: NamingStrategy, PrepareRow, ResultRow, Session, Ctx <: Context[_, _]](val idiom: D, val naming: N):
    def op[I, T, Res] =
      ContextOperation.Single[I, T, Nothing, D, N, PrepareRow, ResultRow, Session, Ctx, Res](idiom, naming)
    def batch[I, T, A <: QAC[I, T] with Action[I], Res] =
      ContextOperation.Batch[I, T, A, D, N, PrepareRow, ResultRow, Session, Ctx, Res](idiom, naming)

/** Enums and helper methods for QueryExecution and QueryExecutionBatch */
object Execution:

  enum ExtractBehavior:
    case Extract
    case ExtractWithReturnAction
    case Skip

  enum ElaborationBehavior:
    case Elaborate
    case Skip
  given ToExpr[ElaborationBehavior] with
    def apply(eb: ElaborationBehavior)(using Quotes) =
      import quotes.reflect._
      eb match
        case ElaborationBehavior.Elaborate => '{ ElaborationBehavior.Elaborate }
        case ElaborationBehavior.Skip      => '{ ElaborationBehavior.Skip }

  // Simple ID function that we use in a couple of places
  def identityConverter[T: Type](using Quotes) = '{ (t: T) => t }

  /** Summon decoder for a given Type and Row type (ResultRow) */
  def summonDecoderOrThrow[ResultRow: Type, Session: Type, DecoderT: Type]()(using Quotes): Expr[GenericDecoder[ResultRow, Session, DecoderT, DecodingType]] =
    import quotes.reflect.{Try => _, _}
    // First try summoning a specific encoder, if that doesn't work, use the generic one.
    // Note that we could do Expr.summon[GenericDecoder[..., DecodingType.Generic]] to summon it
    // but if we do that an error is thrown via report.throwError during summoning then it would just be not summoned and the
    // and no error would be returned to the user. Therefore it is better to just invoke the method here.
    Expr.summon[GenericDecoder[ResultRow, Session, DecoderT, DecodingType.Specific]] match
      case Some(decoder) => decoder
      case None =>
        GenericDecoder.summon[DecoderT, ResultRow, Session]

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
      '{ (r: ResultRow, s: Session) => $contramap.apply(${ decoder }.apply(0, r, s)) }

    def static(state: StaticState, converter: Expr[RawT => T], extract: ExtractBehavior)(using Quotes): Expr[io.getquill.context.Extraction[ResultRow, Session, T]] =
      extract match
        // TODO Allow passing in a starting index here?
        case ExtractBehavior.Extract =>
          val extractor = makeExtractorFrom(converter)
          '{ Extraction.Simple($extractor) }
        case ExtractBehavior.ExtractWithReturnAction =>
          val extractor = makeExtractorFrom(converter)
          val returnAction = state.returnAction.getOrElse { throw new IllegalArgumentException(s"Return action could not be found in the Query: ${query}") }
          '{ Extraction.Returning($extractor, ${ io.getquill.parser.Lifter.returnAction(returnAction) }) }
        case ExtractBehavior.Skip =>
          '{ Extraction.None }

    def dynamic(converter: Expr[RawT => T], extract: ExtractBehavior)(using Quotes): Expr[io.getquill.context.Extraction[ResultRow, Session, T]] =
      extract match
        case ExtractBehavior.Extract =>
          val extractor = makeExtractorFrom(converter)
          '{ Extraction.Simple($extractor) }
        // if a return action is needed, that ReturnAction will be calculated later in the dynamic context
        // therefore here, all we do is to pass in a extractor. We will compute the Extraction.ReturningLater
        case ExtractBehavior.ExtractWithReturnAction =>
          val extractor = makeExtractorFrom(converter)
          '{ Extraction.Simple($extractor) }
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
      // Output type of the Quoted. E.g. People for query[People] or List[People] for query[People].returningMany(p => p)
      // Also when a QueryMeta[OutputT, RawT] is used (e.g. `QueryMeta[PersonName, String]: queryMeta(Query[PersonName] => String)(String => PersonName)`)
      // then OutputT is the output type that gets converted out from RawT.
      T: Type,
      ResultRow: Type,
      PrepareRow: Type,
      Session: Type,
      D <: Idiom: Type,
      N <: NamingStrategy: Type,
      Ctx <: Context[_, _]: Type,
      Res: Type
  ](
      quotedOp: Expr[Quoted[QAC[_, _]]],
      contextOperation: Expr[ContextOperation.Single[I, T, Nothing, D, N, PrepareRow, ResultRow, Session, Ctx, Res]],
      fetchSize: Expr[Option[Int]],
      wrap: Expr[OuterSelectWrap]
  )(using val qctx: Quotes, QAC: Type[QAC[_, _]]):
    import qctx.reflect.{Try => _, _}
    import Execution._

    val transpileConfig = SummonTranspileConfig()
    val interp = new Interpolator(TraceType.Execution, transpileConfig.traceConfig, 1)
    import interp._

    def apply() =
      // Since QAC type doesn't have the needed info (i.e. it's parameters are existential) hence
      // it cannot be checked if they are nothing etc... so instead we need to check the type
      // on the actual quoted term.
      quotedOp.asTerm.tpe.asType match
        // Query has this shape
        case '[Quoted[QAC[Nothing, _]]] => applyQuery(quotedOp)
        // Insert / Delete / Update have this shape
        case '[Quoted[QAC[_, Nothing]]] => applyAction(quotedOp)
        // Insert Returning, ReturningMany, etc... has this shape
        case '[Quoted[QAC[_, _]]] =>
          if (!(TypeRepr.of[T] =:= TypeRepr.of[Any]))
            applyActionReturning(quotedOp) // ReturningAction is also a subtype of Action so check it before Action
          else
            // In certain situations (i.e. if a user does sql"stuff".as[Action[Stuff]] something will be directly specified
            // as an Action[T] without there being a `& QAC[T, Nothing]` as part of the type. In that case, the `ModificationEntity`
            // will just be `Any`. We need to manually detect that case since it requires no return type)
            applyAction(quotedOp)
        case _ =>
          report.throwError(s"Could not match type type of the quoted operation: ${io.getquill.util.Format.Type(QAC)}")

    lazy val wrapValue = OuterSelectWrap.unlift(wrap)
    lazy val queryElaborationBehavior =
      wrapValue match
        case OuterSelectWrap.Always  => ElaborationBehavior.Elaborate
        case OuterSelectWrap.Never   => ElaborationBehavior.Skip
        case OuterSelectWrap.Default => ElaborationBehavior.Elaborate

    /**
     * Summon all needed components and run executeQuery method
     * (Experiment with catching `StaticTranslationMacro.apply` errors since they usually happen
     * because some upstream construct has done a reportError so we do not want to do another one.
     * I.e. if we do another returnError here it will override that one which is not needed.
     * if this seems to work well, make the same change to other apply___ methods here.
     * )
     */
    def applyQuery(quoted: Expr[Quoted[QAC[_, _]]]): Expr[Res] =
      val topLevelQuat = QuatMaking.ofType[T]
      summonQueryMetaTypeIfExists[T] match
        // Can we get a QueryMeta? Run that pipeline if we can
        case Some(queryMeta) =>
          queryMeta match { case '[rawT] => runWithQueryMeta[rawT](quoted) }
        case None =>
          Try(StaticTranslationMacro[D, N](quoted, queryElaborationBehavior, topLevelQuat)) match
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
              executeStatic[T](staticState, identityConverter, ExtractBehavior.Extract, topLevelQuat) // Yes we can, do it!
            case scala.util.Success(None) =>
              executeDynamic(quoted, identityConverter, ExtractBehavior.Extract, queryElaborationBehavior, topLevelQuat) // No we can't. Do dynamic

    def applyAction(quoted: Expr[Quoted[QAC[_, _]]]): Expr[Res] =
      StaticTranslationMacro[D, N](quoted, ElaborationBehavior.Skip, Quat.Value) match
        case Some(staticState) =>
          executeStatic[T](staticState, identityConverter, ExtractBehavior.Skip, Quat.Value)
        case None =>
          executeDynamic(quoted, identityConverter, ExtractBehavior.Skip, ElaborationBehavior.Skip, Quat.Value)

    def applyActionReturning(quoted: Expr[Quoted[QAC[_, _]]]): Expr[Res] =
      val topLevelQuat = QuatMaking.ofType[T]
      StaticTranslationMacro[D, N](quoted, ElaborationBehavior.Skip, topLevelQuat) match
        case Some(staticState) =>
          executeStatic[T](staticState, identityConverter, ExtractBehavior.ExtractWithReturnAction, topLevelQuat)
        case None =>
          executeDynamic(quoted, identityConverter, ExtractBehavior.ExtractWithReturnAction, ElaborationBehavior.Skip, Quat.Value)

    /** Run a query with a given QueryMeta given by the output type RawT and the conversion RawT back to OutputT */
    def runWithQueryMeta[RawT: Type](quoted: Expr[Quoted[QAC[_, _]]]): Expr[Res] =
      val topLevelQuat = QuatMaking.ofType[RawT]
      val (queryRawT, converter, staticStateOpt) = QueryMetaExtractor.applyImpl[T, RawT, D, N](quoted.asExprOf[Quoted[Query[T]]], topLevelQuat)
      staticStateOpt match {
        case Some(staticState) =>
          executeStatic[RawT](staticState, converter, ExtractBehavior.Extract, topLevelQuat)
        case None =>
          // Note: Can assume QuotationType is `Query` here since summonly a Query-meta is only allowed for Queries
          // Also: A previous implementation of this used QAC[I, T] => QAC[I, RawT] directly but was scrapped due to some Dotty issues
          // that later got fixed. If this implementation becomes cumbersome we can try that.
          executeDynamic[RawT](queryRawT.asExprOf[Quoted[QAC[I, RawT]]], converter, ExtractBehavior.Extract, queryElaborationBehavior, topLevelQuat)
      }

    def resolveLazyLiftsStatic(lifts: List[Expr[Planter[?, ?, ?]]]): List[Expr[Planter[?, ?, ?]]] =
      import io.getquill.metaprog.{LazyPlanterExpr, EagerPlanterExpr}
      lifts.map {
        case '{ ($e: EagerPlanter[a, b, c]) }     => e
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
    def executeStatic[RawT: Type](state: StaticState, converter: Expr[RawT => T], extract: ExtractBehavior, topLevelQuat: Quat): Expr[Res] =
      val lifts = resolveLazyLiftsStatic(state.lifts)
      trace"Original Lifts (including lazy): ${state.lifts.map(_.show)} resoved to: ${lifts.map(_.show)}".andLog()

      // Create the row-preparer to prepare the SQL Query object (e.g. PreparedStatement)
      // and the extractor to read out the results (e.g. ResultSet)
      val prepare = '{ (row: PrepareRow, session: Session) => LiftsExtractor.apply[PrepareRow, Session](${ Expr.ofList(lifts) }, row, session) }
      val extractor = MakeExtractor[ResultRow, Session, T, RawT].static(state, converter, extract)

      val emptyContainsTokenExpr: Expr[Token => Token] = '{ $contextOperation.idiom.emptySetContainsToken(_) }
      val liftingPlaceholderExpr: Expr[Int => String] = '{ $contextOperation.idiom.liftingPlaceholder }
      val particularQuery = Particularize.Static(state.query, lifts, liftingPlaceholderExpr, emptyContainsTokenExpr, '{ 1 })(transpileConfig.traceConfig)
      // Plug in the components and execute
      val astSplice =
        if (TypeRepr.of[Ctx] <:< TypeRepr.of[AstSplicing]) Lifter(state.ast)
        else '{ io.getquill.ast.NullValue }
      '{ $contextOperation.execute(ContextOperation.SingleArgument($particularQuery, $prepare, $extractor, ExecutionInfo(ExecutionType.Static, $astSplice, ${ Lifter.quat(topLevelQuat) }), $fetchSize)) }
    end executeStatic

    /**
     * Expand dynamic-queries i.e. queries whose query-string cannot be computed at compile-time.
     * Note that for now, QuotationType is only needed for dynamic queries (which is only needed to know whether you
     * need to use ElaborateStructure or not. This is decided in the StaticTranslationMacro for static queries using a
     * different method. I.e. since StaticTranslationMacro knows the AST node it infers Action/Query from that).
     */
    def executeDynamic[RawT: Type](quote: Expr[Quoted[QAC[?, ?]]], converter: Expr[RawT => T], extract: ExtractBehavior, elaborationBehavior: ElaborationBehavior, topLevelQuat: Quat) =
      // Grab the ast from the quote and make that into an expression that we will pass into the dynamic evaluator
      // Expand the outermost quote using the macro and put it back into the quote
      // Is the expansion on T or RawT, need to investigate
      // Note that in the Static case, this is done by checking the Type of the root entity. If the root Entity
      // is a Query, then the Elaboration happens. For the dynamic query variation, this is more difficult to do because
      // we have Expr[Ast] instead of Ast in the times when we have Type[T]. We could Elaborate T during compile-time
      // and then decide to plug in the elaboration or not during runtime (depending on the type of Ast which would be runtime-checked)
      // but that would be less straightforward to do.
      val pluckedAst = '{ $quote.ast }
      val elaboratedAstQuote = '{ $quote.copy(ast = $pluckedAst) }
      val extractor: Expr[io.getquill.context.Extraction[ResultRow, Session, T]] = MakeExtractor[ResultRow, Session, T, RawT].dynamic(converter, extract)

      // TODO What about when an extractor is not neededX
      val spliceAsts = TypeRepr.of[Ctx] <:< TypeRepr.of[AstSplicing]
      // Note, we don't want to serialize the Quat here because it is directly spliced into the execution method call an only once.
      // / For the sake of viewing/debugging the quat macro code it is better not to serialize it here
      '{
        RunDynamicExecution.apply[I, T, RawT, D, N, PrepareRow, ResultRow, Session, Ctx, Res](
          $elaboratedAstQuote,
          $contextOperation,
          $extractor,
          ${ Expr(spliceAsts) },
          $fetchSize,
          ${ Expr(elaborationBehavior) },
          ${ Lifter.NotSerializing.quat(topLevelQuat) },
          ${ TranspileConfigLiftable(transpileConfig) }
        )
      }
    end executeDynamic

  end RunQuery

  inline def apply[
      I,
      T,
      DecodeT,
      ResultRow,
      PrepareRow,
      Session,
      D <: Idiom,
      N <: NamingStrategy,
      Ctx <: Context[_, _],
      Res
  ](ctx: ContextOperation.Single[I, T, Nothing, D, N, PrepareRow, ResultRow, Session, Ctx, Res])(inline quotedOp: Quoted[QAC[_, _]], fetchSize: Option[Int], inline wrap: OuterSelectWrap = OuterSelectWrap.Default) =
    ${ applyImpl('quotedOp, 'ctx, 'fetchSize, 'wrap) }

  def applyImpl[
      I: Type,
      T: Type,
      DecodeT: Type,
      ResultRow: Type,
      PrepareRow: Type,
      Session: Type,
      D <: Idiom: Type,
      N <: NamingStrategy: Type,
      Ctx <: Context[_, _]: Type,
      Res: Type
  ](
      quotedOp: Expr[Quoted[QAC[_, _]]],
      ctx: Expr[ContextOperation.Single[I, T, Nothing, D, N, PrepareRow, ResultRow, Session, Ctx, Res]],
      fetchSize: Expr[Option[Int]],
      wrap: Expr[OuterSelectWrap]
  )(using qctx: Quotes): Expr[Res] = new RunQuery[I, T, ResultRow, PrepareRow, Session, D, N, Ctx, Res](quotedOp, ctx, fetchSize, wrap).apply()

end QueryExecution

object PrepareDynamicExecution:
  import io.getquill.idiom.{Idiom => Idiom}
  import io.getquill.{NamingStrategy => NamingStrategy}
  import io.getquill.idiom.Statement
  import io.getquill.ast.ReturningAction
  import io.getquill.context.Execution.ElaborationBehavior

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
      elaborationBehavior: ElaborationBehavior,
      topLevelQuat: Quat,
      transpileConfig: TranspileConfig,
      spliceBehavior: SpliceBehavior = SpliceBehavior.NeedsSplice,
      // For a batch query, these are the other lifts besides the primary liftQuery lifts.
      // This should be empty & ignored for all other query types.
      additionalLifts: List[Planter[?, ?, ?]] = List(),
      batchAlias: Option[String] = None
  ) =
    // Splice all quotation values back into the AST recursively, by this point these quotations are dynamic
    // which means that the compiler has not done the splicing for us. We need to do this ourselves.
    // So we need to go through all the QuotationTags in the AST and splice in the corresponding QuotationVase into it's place.
    // (also, we need to tell if ReturningGenerated is the top-level element in order to know that the
    // extraction type is Extraction.Returning by in some cases the AST will be
    // FunctionApply(Function(ident, ReturningGenerated(...))), stuff). In those cases, we need
    // to do a beta-reduction first.
    val (splicedAstRaw, gatheredLifts) =
      spliceBehavior match
        case SpliceBehavior.NeedsSplice    => (spliceQuotations(quoted), gatherLifts(quoted))
        case SpliceBehavior.AlreadySpliced => (quoted.ast, quoted.lifts) // If already spliced, can skip all runtimeQuotes clauses since their asts have already been spliced, same with lifts

    VerifyFreeVariables.runtime(splicedAstRaw)

    val splicedAst = ElaborateTrivial(elaborationBehavior)(splicedAstRaw)

    // TODO Should make this enable-able via a logging configuration
    // println("=============== Dynamic Expanded Ast Is ===========\n" + io.getquill.util.Messages.qprint(splicedAst))

    // Tokenize the spliced AST
    val queryType = IdiomContext.QueryType.discoverFromAst(splicedAst, batchAlias)
    val idiomContext = IdiomContext(transpileConfig, queryType)
    val (outputAst, stmt, _) = idiom.translate(splicedAst, topLevelQuat, ExecutionType.Dynamic, idiomContext)(using naming)
    val naiveQury = Unparticular.translateNaive(stmt, idiom.liftingPlaceholder)

    val liftColumns =
      (ast: Ast, stmt: Statement) => Unparticular.translateNaive(stmt, idiom.liftingPlaceholder)

    val returningActionOpt =
      splicedAst match
        // If we have a returning action, we need to compute some additional information about how to return things.
        // Different database dialects handle these things differently. Some allow specifying a list of column-names to
        // return from the query. Others compute this information from the query data directly. This information is stored
        // in the dialect and therefore is computed here.
        case returningActionAst: ReturningAction =>
          Some(io.getquill.norm.ExpandReturning.applyMap(returningActionAst)(liftColumns)(idiom, naming, idiomContext))
        case _ =>
          None

    val extractor = (rawExtractor, returningActionOpt) match
      case (Extraction.Simple(extract), Some(returningAction)) => Extraction.Returning(extract, returningAction)
      case (Extraction.Simple(_), None)                        => rawExtractor
      case (Extraction.None, None)                             => rawExtractor
      case (extractor, returningAction)                        => throw new IllegalArgumentException(s"Invalid state. Cannot have ${extractor} with a returning action ${returningAction}")

    val (_, externals) = Unparticular.Query.fromStatement(stmt, idiom.liftingPlaceholder)

    // Get the UIDs from the lifts, if they are something unexpected (e.g. Lift elements from Quill 2.x) throw an exception
    val liftTags =
      externals.map {
        case tag @ ScalarTag(_, _) => tag
        case other                 => throw new IllegalArgumentException(s"Invalid Lift Tag: ${other}")
      }

    // Match the ScalarTags we pulled out earlier (in ReifyStatement) with corresponding Planters because
    // the Planters can be out of order (I.e. in a different order then the ?s in the SQL query that they need to be spliced into).
    // The ScalarTags are coming directly from the tokenized AST however and their order should be correct.
    // also, some of they may be filtered out
    val (sortedLifts, sortedSecondaryLifts) =
      processLifts(gatheredLifts, liftTags, additionalLifts) match
        case Right((sl, ssl)) => (sl, ssl)
        case Left(msg) =>
          throw new IllegalArgumentException(
            s"Could not process the lifts:\n" +
              s"${gatheredLifts.map(_.toString).mkString("====\n")}" +
              (if (additionalLifts.nonEmpty) s"${additionalLifts.map(_.toString).mkString("====\n")}" else "") +
              s"Due to an error: $msg"
          )

    (stmt, outputAst, sortedLifts, extractor, sortedSecondaryLifts)

  end apply

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

  private[getquill] def processLifts(
      lifts: List[Planter[_, _, _]],
      matchingExternals: List[External],
      secondaryLifts: List[Planter[_, _, _]] = List()
  ): Either[String, (List[Planter[_, _, _]], List[Planter[_, _, _]])] =
    val encodeablesMap =
      lifts.map(e => (e.uid, e)).toMap

    val secondaryEncodeablesMap =
      secondaryLifts.map(e => (e.uid, e)).toMap

    val uidsOfScalarTags =
      matchingExternals.collect {
        case tag: ScalarTag => tag.uid
      }

    enum UidStatus:
      // Most normal lifts and the liftQuery of batches
      case Primary(uid: String, planter: Planter[?, ?, ?])
      // In batch queries, any lifts that are not part of the initial liftQuery
      case Secondary(uid: String, planter: Planter[?, ?, ?])
      // Lift planter was not found, this means an error
      case NotFound(uid: String)
      def print: String = this match
        case Primary(uid, planter)   => s"PrimaryPlanter($uid, ${planter})"
        case Secondary(uid, planter) => s"SecondaryPlanter($uid, ${planter})"
        case NotFound(uid)           => s"NotFoundPlanter($uid)"

    val sortedEncodeables =
      uidsOfScalarTags
        .map { uid =>
          encodeablesMap.get(uid) match
            case Some(element) => UidStatus.Primary(uid, element)
            case None =>
              secondaryEncodeablesMap.get(uid) match
                case Some(element) => UidStatus.Secondary(uid, element)
                case None          => UidStatus.NotFound(uid)
        }

    object HasNotFoundUids:
      def unapply(statuses: List[UidStatus]) =
        val collected =
          statuses.collect {
            case UidStatus.NotFound(uid) => uid
          }
        if (collected.nonEmpty) Some(collected) else None

    object PrimaryThenSecondary:
      def unapply(statuses: List[UidStatus]) =
        val (primaries, secondaries) =
          statuses.partition {
            case UidStatus.Primary(_, _) => true
            case _                       => false
          }
        val primariesFound = primaries.collect { case p: UidStatus.Primary => p }
        val secondariesFound = secondaries.collect { case s: UidStatus.Secondary => s }
        val goodPartitioning =
          primariesFound.length == primaries.length && secondariesFound.length == secondaries.length
        if (goodPartitioning)
          Some((primariesFound.map(_.planter), secondariesFound.map(_.planter)))
        else
          None

    val outputEncodeables =
      sortedEncodeables match
        case HasNotFoundUids(uids) =>
          Left(s"Invalid Transformations Encountered. Cannot find lift with IDs: ${uids}.")
        case PrimaryThenSecondary(primaryPlanters, secondaryPlanters /*or List() if none*/ ) =>
          Right((primaryPlanters, secondaryPlanters))
        case other =>
          Left(
            s"Invalid transformation primary and secondary encoders were mixed.\n" +
              s"All secondary planters must come after all primary ones but found:\n" +
              s"${other.map(_.print).mkString("=====\n")}"
          )

    // TODO This should be logged if some fine-grained debug logging is enabled. Maybe as part of some phase that can be enabled via -Dquill.trace.types config
    // val remaining = encodeables.removedAll(uidsOfScalarTags)
    // if (!remaining.isEmpty)
    //   println(s"Ignoring the following lifts: [${remaining.map((_, v) => Format.Expr(v.plant)).mkString(", ")}]")
    outputEncodeables
  end processLifts

end PrepareDynamicExecution

/**
 * Drives dynamic execution from the Context
 * Note that AST is already elaborated by the time it comes into here
 */
object RunDynamicExecution:

  import io.getquill.idiom.{Idiom => Idiom}
  import io.getquill.{NamingStrategy => NamingStrategy}
  import io.getquill.idiom.Statement
  import io.getquill.ast.ReturningAction
  import io.getquill.context.Execution.ElaborationBehavior

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
  ](
      quoted: Quoted[QAC[I, RawT]],
      ctx: ContextOperation.Single[I, T, Nothing, D, N, PrepareRow, ResultRow, Session, Ctx, Res],
      rawExtractor: Extraction[ResultRow, Session, T],
      spliceAst: Boolean,
      fetchSize: Option[Int],
      elaborationBehavior: ElaborationBehavior,
      topLevelQuat: Quat,
      transpileConfig: TranspileConfig
  ): Res = {
    // println("===== Passed Ast: " + io.getquill.util.Messages.qprint(quoted.ast))
    val (stmt, outputAst, sortedLifts, extractor, sortedSecondaryLifts) =
      PrepareDynamicExecution[I, T, RawT, D, N, PrepareRow, ResultRow, Session](quoted, rawExtractor, ctx.idiom, ctx.naming, elaborationBehavior, topLevelQuat, transpileConfig)

    // Turn the Tokenized AST into an actual string and pull out the ScalarTags (i.e. the lifts)
    val (unparticularQuery, _) = Unparticular.Query.fromStatement(stmt, ctx.idiom.liftingPlaceholder)
    // TODO don't really need lift-sorting in PrepareDynamicExecution anymore? Could use liftsOrderer to do that
    val (queryString, _) = Particularize.Dynamic(unparticularQuery, sortedLifts ++ sortedSecondaryLifts, ctx.idiom.liftingPlaceholder, ctx.idiom.emptySetContainsToken)(transpileConfig.traceConfig)

    // Use the sortedLifts to prepare the method that will prepare the SQL statement
    val prepare = (row: PrepareRow, session: Session) => LiftsExtractor.Dynamic[PrepareRow, Session](sortedLifts, row, session)

    // Exclute the SQL Statement
    val executionAst = if (spliceAst) outputAst else io.getquill.ast.NullValue
    ctx.execute(ContextOperation.SingleArgument(queryString, prepare, extractor, ExecutionInfo(ExecutionType.Dynamic, executionAst, topLevelQuat), fetchSize))
  }

end RunDynamicExecution
