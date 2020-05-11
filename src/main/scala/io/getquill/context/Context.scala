package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.dsl.Dsl
//import io.getquill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import miniquill.quoter.Query
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
import scala.quoted.{Type => TType, _}
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationBin
import miniquill.quoter.QuotedExpr
import miniquill.quoter.ScalarPlanterExpr
import io.getquill.idiom.ReifyStatement

import io.getquill._

sealed trait ExecutionType
object ExecutionType {
  case object Dynamic extends ExecutionType
  case object Static extends ExecutionType
}

  


object QueryMetaExtractor {
  import miniquill.parser._
  import scala.quoted._ // summonExpr is actually from here
  import scala.quoted.matching._ // ... or from here
  import miniquill.quoter.ScalarPlanter
  import miniquill.quoter._
  import io.getquill.ast.FunctionApply

  inline def run[T, R, D <: io.getquill.idiom.Idiom, N <: io.getquill.NamingStrategy](
    inline quotedRaw: Quoted[Query[T]],
    inline ctx: Context[D, N]
  ): (Quoted[Query[R]], R => T, Option[(String, List[ScalarPlanter[_, _]])]) = 
    ${ runImpl[T, R, D, N]('quotedRaw, 'ctx) }

  def runImpl[T: Type, R: Type, D <: io.getquill.idiom.Idiom: Type, N <: io.getquill.NamingStrategy: Type](
    quotedRaw: Expr[Quoted[Query[T]]],
    ctx: Expr[Context[D, N]]
  )(given qctx:QuoteContext): Expr[(Quoted[Query[R]], R => T, Option[(String, List[ScalarPlanter[_,_]])])] = {
    import qctx.tasty.{Try => TTry, _, given _}
    val quotedArg = quotedRaw.unseal.underlyingArgument.seal.cast[Quoted[Query[T]]]

    summonExpr(given '[QueryMeta[T, R]]) match {
      case Some(queryMeta) =>  
        queryMeta match {
          case '{ ($qmm: QueryMeta[T, R]) } =>

            val inlineQuoteOpt = QuotedExpr.inlineWithListOpt(quotedArg)
            (inlineQuoteOpt, qmm) match {
              // TODO Need a case where these are not matched
              case (Some((quotedExr, quotedExprLifts)), QuotationBinExpr.InlineOrPlucked(quotationBin)) =>
                println("~~~~~~~~~~~~~~~~~~~~~~~ Matched Quote Meta ~~~~~~~~~~~~~~~~~~~~~~~")

                quotationBin match {
                  // todo try astMappingFunc rename to `Ast(T => r)` or $r
                  case InlineableQuotationBinExpr(uid, astMappingFunc, _, quotation, lifts, List(extractor)) => 
                    // Don't need to unlift the ASTs and re-lift them. Just put them into a FunctionApply
                    val astApply = 
                      '{FunctionApply($astMappingFunc, List(${quotedExr.ast}))}

                    // TODO Dedupe?
                    val newLifts = (lifts ++ quotedExprLifts).map(_.plant)

                    

                    // In the compile-time case, we can synthesize the new quotation
                    // much more easily since we can just combine the lifts and Apply the
                    // QueryMeta quotation to the initial one
                    
                    // Syntheize a new quotation to combine the lifts and quotes of the reapplied
                    // query. I do not want to use QuoteMacro here because that requires a parser
                    // which means that the Context will require a parser as well. That will
                    // make the parser harder to customize by users
                    val reappliedQuery =
                      '{ Quoted[Query[R]]($astApply, ${Expr.ofList(newLifts)}, List()) }

                    val extractorFunc = '{ $extractor.asInstanceOf[R => T] }

                    val staticTranslation = StaticTranslationMacro[R, D, N](reappliedQuery, ctx)

                    '{ ($reappliedQuery, $extractorFunc, $staticTranslation) }

                    //StaticTranslationMacro[R, D, N](reappliedQuery, context)
                    
                  case PluckableQuotationBinExpr(uid, astTree, _) => 
                    qctx.throwError("Runtime-only query schemas are not allowed for now")
                }
            }

            //printer.lnf(r.unseal)
          case _ =>
            println("~~~~~~~~~~~~~~~~~~~~~~~ NOT Matched Quote Meta ~~~~~~~~~~~~~~~~~~~~~~~")
            // TODO Should probably check them one at a time to see if they are correct
            qctx.throwError("Invalid Quotation or Quote Meta")
        }
        
        //qctx.throwError("Quote Meta Identified but not found!")
      case None => 
        println("=============== Query Meta NOT FOUND! ============")
        qctx.throwError("Quote Meta needed but not found!")
    }
  }
}

// TODO Needs to be portable (i.e. plug into current contexts when compiled with Scala 3)
trait Context[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy]
extends EncodingDsl
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

  protected val identityPrepare: Prepare = (Nil, _)
  protected val identityExtractor = identity[ResultRow] _

  inline def translateStatic[T](inline quoted: Quoted[Query[T]]): Option[(String, List[ScalarPlanter[_, _]])] =
    ${ StaticTranslationMacro[T, Dialect, Naming]('quoted, 'self) }

  inline def lift[T](inline vv: T): T = 
    ${ LiftMacro[T, PrepareRow]('vv) }


    
  inline def runDynamic[RawT, T](inline quoted: Quoted[Query[RawT]], inline decoder: GenericDecoder[_, RawT], inline converter: RawT => T): Result[RunQueryResult[T]] = {
    val ast = Expander.runtime[RawT](quoted.ast)
    val lifts = quoted.lifts
    val quotationVases = quoted.runtimeQuotes

    def spliceQuotations(ast: Ast): Ast =
      Transform(ast) {
        case v @ QuotationTag(uid) => 
          // When a quotation to splice has been found, retrieve it and continue
          // splicing inside since there could be nested sections that need to be spliced
          quotationVases.find(_.uid == uid) match {
            case Some(vase) => 
              spliceQuotations(vase.quoted.ast)
            // TODO Macro error if a uid can't be looked up (also show all uid secionds that currently exist)
          }
      }

    val expandedAst = spliceQuotations(ast)
      
    val (outputAst, stmt) = idiom.translate(expandedAst)(given naming)

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
    val extractor = (r: ResultRow) => converter(decoder.asInstanceOf[GenericDecoder[ResultRow, RawT]].apply(1, r))
    this.executeQuery(string, null, extractor, ExecutionType.Dynamic)
  }

  inline def summonDecoder[T]: GenericDecoder[ResultRow, T] = ${ SummonDecoderMacro[T, ResultRow] }

  //inline def run[T](inline qry: Query[T])(implicit quoter: miniquill.quoter.Quoter): Result[RunQueryResult[T]] =
  //  run(quoter.quote(qry))

  def staticExtractor(lifts: List[ScalarPlanter[_, _]], row: PrepareRow) = {
    val (_, values, prepare) =
      lifts.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val newRow = 
            lift
            .asInstanceOf[ScalarPlanter[Any, PrepareRow]]
            .encoder(idx, lift.value, row).asInstanceOf[PrepareRow] // TODO since summoned encoders are casted
          (idx + 1, lift.value :: values, newRow)
      }
    (values, prepare)
  }

  // Run the query with a queryString as well as extractors. If the final row is different from the row
  // that is being decoded (they will be different unless a QueryMeta changes it) then run the
  // additional converter on every row.
  
  inline def runStatic[RawT,T](queryString: String, lifts: List[ScalarPlanter[_, _]], decoder: GenericDecoder[_, RawT], converter: RawT => T) = {
    val extractor = (r: ResultRow) => converter(decoder.asInstanceOf[GenericDecoder[ResultRow, RawT]].apply(1, r))
    val prepare = (row: PrepareRow) => staticExtractor(lifts, row)

    this.executeQuery(queryString, prepare, extractor, ExecutionType.Static)
  }

  

  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    summonFrom {
      case qm: QueryMeta[T, someR] =>
        val (reappliedQuery, converter, staticState) = QueryMetaExtractor.run[T, someR, Dialect, Naming](quoted, this)
        encodeAndExecute[T, someR](staticState, reappliedQuery, converter)
      case _ => 
        val staticState = translateStatic[T](quoted)
        encodeAndExecute[T, T](staticState, quoted, t => t)
    }
  }

  inline def encodeAndExecute[T, R](
    inline staticState: Option[(String, List[ScalarPlanter[_, _]])], 
    inline quoted: Quoted[Query[R]],
    inline converter: R => T): Result[RunQueryResult[T]] = 
  {
    staticState match {
      case Some((query, lifts)) => // use FindLifts as part of this match?
        // val decoder =
        //   summonFrom {
        //     case decoder: GenericDecoder[ResultRow, R] => decoder
        //     // TODO Extract into meta function, have a good error if decoder not found
        //   }
        val decoder = summonDecoder[R]
        runStatic[R, T](query, lifts, decoder, converter)

      case None =>
        val decoder = summonDecoder[R]
        runDynamic[R, T](quoted, decoder, converter)
    }
  }
}

object SummonDecoderMacro {
  import miniquill.parser._
  import scala.quoted._ // summonExpr is actually from here
  import scala.quoted.matching._ // ... or from here
  import miniquill.quoter.ScalarPlanter

  def apply[T: Type, ResultRow: Type](given qctx: QuoteContext): Expr[GenericDecoder[ResultRow, T]] = {
    import qctx.tasty.{Type => TType, given, _}
    summonExpr(given '[GenericDecoder[ResultRow, T]]) match {
      case Some(decoder) => decoder
      case None => qctx.throwError(s"Cannot Find decoder for ${summon[Type[T]].show}")
    }
  }  
}

object LiftMacro {
  import scala.quoted._ // summonExpr is actually from here
  import scala.quoted.matching._ // ... or from here
  import miniquill.quoter.ScalarPlanter
  import miniquill.dsl.GenericEncoder

  def apply[T, PrepareRow](vvv: Expr[T])(given qctx: QuoteContext, tType: TType[T], prepareRowType: TType[PrepareRow]): Expr[T] = {
    import qctx.tasty.{given, _}
    val uuid = java.util.UUID.randomUUID().toString
    val encoder = 
      summonExpr(given '[GenericEncoder[$tType, $prepareRowType]]) match {
        case Some(enc) => enc
        case None => qctx.throwError(s"Cannot Find encode for ${tType.unseal}", vvv)
      }
    '{ ScalarPlanter($vvv, $encoder, ${Expr(uuid)}).unquote } //[$tType, $prepareRowType] // adding these causes assertion failed: unresolved symbols: value Context_this
  }
}

object StaticTranslationMacro {
  import miniquill.parser._
  import scala.quoted._ // summonExpr is actually from here
  import scala.quoted.matching._ // ... or from here
  import miniquill.quoter.ScalarPlanter
  import io.getquill.idiom.LoadNaming
  import io.getquill.util.LoadObject
  import miniquill.dsl.GenericEncoder
  import io.getquill.ast.External

  // Process the AST during compile-time. Return `None` if that can't be done.
  private[getquill] def processAst[T: Type](astExpr: Expr[Ast], idiom: Idiom, naming: NamingStrategy)(given qctx: QuoteContext):Option[(String, List[External])] = {
    import io.getquill.ast.{CollectAst, QuotationTag}

    def noRuntimeQuotations(ast: Ast) =
      CollectAst.byType[QuotationTag](ast).isEmpty

    // val queryMeta = 
    //   summonExpr(given '[QueryMeta])

    val unliftedAst = new Unlifter(given qctx).apply(astExpr)
    if (noRuntimeQuotations(unliftedAst)) {
      val expandedAst = Expander.static[T](unliftedAst) 
      val (ast, stmt) = idiom.translate(expandedAst)(given naming)
      val output =
        ReifyStatement(
          idiom.liftingPlaceholder,
          idiom.emptySetContainsToken,
          stmt,
          forProbing = false
        )
      Some(output)
    } else {
      None
    }
  }

  // Process compile-time lifts, return `None` if that can't be done.
  // liftExprs = Lifts that were put into planters during the quotation. They are
  // 're-planted' back into the PreparedStatement vars here.
  // matchingExternals = the matching placeholders (i.e 'lift tags') in the AST 
  // that contains the UUIDs of lifted elements. We check against list to make
  // sure that that only needed lifts are used and in the right order.
  private[getquill] def processLifts(liftExprs: Expr[List[ScalarPlanter[_, _]]], matchingExternals: List[External])(given qctx: QuoteContext): Option[List[Expr[ScalarPlanter[_, _]]]] = {
    val extractedEncodeables =
      liftExprs match {
        case ScalarPlanterExpr.InlineList(lifts) =>
          // get all existing expressions that can be encoded
          Some(lifts.map(e => (e.uid, e)).toMap)
        case _ => 
          // TODO Maybe do ctx.error here to show the lifts to the user, if 'verbose mode' is enabled.
          // Try it out to see how it looks
          println("Lifts do meet compiletime criteria:\n"+liftExprs.show); 
          None
      }

    extractedEncodeables.map { encodeables => 
      matchingExternals.collect {
        case tag: ScalarTag =>
          encodeables.get(tag.uid) match {
            case Some(encodeable) => encodeable
            case None =>
              qctx.throwError(s"Invalid Transformations Encountered. Cannot find lift with ID: ${tag.uid}.")
              // TODO Throw an error here or attempt to resolve encoders during runtime?
              // maybe the user has hand-modified a quoted block and only the
              // lifts are modified with some runtime values?
              // If throwing an error (or maybe even not?) need to tell the user which lifted ids cannot be found
              // should probably add some info to the reifier to "highlight" the question marks that
              // cannot be plugged in. It would also be really nice to show the user which lift-statements
              // are wrong but that requires a bit more thought (maybe match them somehow into the original AST
              // from quotedRow via the UUID???)
          }
      }.map(_.plant) // todo dedupe here?
    }
  }

  def idiomAndNamingStatic[D <: Idiom, N <: NamingStrategy](given qctx: QuoteContext, dialectTpe:TType[D], namingType:TType[N]): Try[(Idiom, NamingStrategy)] =
    for {
      idiom <- LoadObject(dialectTpe)
      namingStrategy <- LoadNaming.static(namingType)
    } yield (idiom, namingStrategy)


  def apply[T: TType, D <: Idiom, N <: NamingStrategy](
    quotedRaw: Expr[Quoted[Query[T]]], context: Expr[Context[D, N]]
  )(given qctx:QuoteContext, dialectTpe:TType[D], namingType:TType[N]): Expr[Option[(String, List[ScalarPlanter[_, _]])]] = {
    import qctx.tasty.{Try => TTry, _, given _}
    // NOTE Can disable if needed and make quoted = quotedRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val quoted = quotedRaw.unseal.underlyingArgument.seal

    val tryStatic =
      for {
        (idiom, naming)          <- idiomAndNamingStatic.toOption
        quotedExpr               <- QuotedExpr.inlineOpt(quoted)
        (queryString, externals) <- processAst[T](quotedExpr.ast, idiom, naming)
        encodedLifts             <- processLifts(quotedExpr.lifts, externals)
      } yield {
        println("Compile Time Query Is: " + queryString)

        // What about a missing decoder?
        // need to make sure that that kind of error happens during compile time
        // (also need to propagate the line number, talk to Li Houyi about that)
        '{ (${Expr(queryString)}, ${Expr.ofList(encodedLifts)}) }
      }

    if (tryStatic.isEmpty)
      println("WARNING: Dynamic Query Detected: ")

    tryStatic match {
      case Some(value) => '{ Option($value) }
      case None        => '{ None }
    }
  }
}




