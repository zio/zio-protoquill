package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.dsl.CoreDsl
//import io.getquill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import miniquill.quoter.Query

import miniquill.dsl.EncodingDsl
import miniquill.quoter.Quoted
import miniquill.quoter.Query
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

// TODO Non Portable
trait Context[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy] 
extends EncodingDsl
//  extends Closeable
//  with CoreDsl 
{
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

  inline def runDynamic[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val ast = Expander.runtime[T](quoted.ast)
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
    val queryString = stmt.toString
    // summon a decoder and a expander (as well as an encoder) all three should be provided by the context
    val decoder = summonDecoder[T]
      // summonFrom {
      //   // TODO Implicit summoning error
      //   case decoder: Decoder[T] => decoder
      // }
    val extractor = (r: ResultRow) => decoder.apply(1, r)
    this.executeQuery(queryString, null, extractor, ExecutionType.Dynamic)
  }

  inline def summonDecoder[T]: Decoder[T] = ${ Context.summonDecoderImpl[T, ResultRow] }

  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val staticQuery = translateStatic[T](quoted)
    staticQuery match {
      case Some((query, lifts)) => // use FindLifts as part of this match?
        val decoder =
          summonFrom {
            case decoder: Decoder[T] => decoder
          }
        val extractor = (r: ResultRow) => decoder.apply(1, r)

        val prepare = 
          (row: PrepareRow) => {
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

        this.executeQuery(query, prepare, extractor, ExecutionType.Static)

      case None =>
        runDynamic(quoted)
    }
  }

  // todo add 'prepare' i.e. encoders here
  def executeQuery[T](sql: String, prepare: Prepare, extractor: Extractor[T], executionType: ExecutionType): Result[RunQueryResult[T]]

  protected val identityPrepare: Prepare = (Nil, _)
  protected val identityExtractor = identity[ResultRow] _

  inline def translateStatic[T](inline quoted: Quoted[Query[T]]): Option[(String, List[ScalarPlanter[_, _]])] =
    ${ Context.translateStaticImpl[T, Dialect, Naming, PrepareRow]('quoted, 'this) }

  inline def lift[T](inline vv: T): T = 
    ${ Context.eagerLiftImpl[T, PrepareRow]('vv) }
}

object Context {
  import miniquill.parser._
  import scala.quoted._ // summonExpr is actually from here
  import scala.quoted.matching._ // ... or from here
  import miniquill.quoter.ScalarPlanter

  // (**) It seems like only unlift is needed here. If a parser needs to be passed into here,
  // extending it is hard (e.g. need the same approach as Literal/Dialect Class.forName stuff)
  // however if all we need is a unlifter which is not designed to be extended, can just
  // reuse it here
  // def parserFactory: (QuoteContext) => PartialFunction[Expr[_], Ast] = 
  //   (qctx: QuoteContext) => new Parser(given qctx)

  // def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
  //   (qctx: QuoteContext) => new Lifter(given qctx)

  import io.getquill.idiom.LoadNaming
  import io.getquill.util.LoadObject
  import miniquill.dsl.GenericEncoder

  //inline def summonDecoder[T]: Decoder[T] = ${ summonDecoderImpl[T] }
  def summonDecoderImpl[T: Type, ResultRow: Type](given qctx: QuoteContext): Expr[GenericDecoder[ResultRow, T]] = {
    import qctx.tasty.{Type => TType, given, _}
    summonExpr(given '[GenericDecoder[ResultRow, T]]) match {
      case Some(decoder) => decoder
      case None => qctx.error(s"Cannot Find decoder for ${summon[Type[T]]}"); '{???}
    }
  }

  def eagerLiftImpl[T, PrepareRow](vvv: Expr[T])(given qctx: QuoteContext, tType: TType[T], prepareRowType: TType[PrepareRow]): Expr[T] = {
    import qctx.tasty.{given, _}
    val uuid = java.util.UUID.randomUUID().toString
    val encoder = 
      summonExpr(given '[GenericEncoder[$tType, $prepareRowType]]) match {
        case Some(enc) => enc
        case None => qctx.error(s"Cannot Find encode for ${tType.unseal}", vvv); '{???}
        // TODO return summoning error if not correct
      }
    '{ ScalarPlanter($vvv, $encoder, ${Expr(uuid)}).unquote } //[$tType, $prepareRowType] // adding these causes assertion failed: unresolved symbols: value Context_this
  }

  class ExpandTags[D <: Idiom, N <: NamingStrategy](ast: Expr[Any])(given qctx: QuoteContext, dialectTpe:TType[D], namingType:TType[N]) {

  }

  def idiomAndNamingStatic[D <: Idiom, N <: NamingStrategy](given qctx: QuoteContext, dialectTpe:TType[D], namingType:TType[N]): Try[(Idiom, NamingStrategy)] =
    for {
      idiom <- LoadObject(dialectTpe)
      namingStrategy <- LoadNaming.static(namingType)
    } yield (idiom, namingStrategy)

  // TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
  def translateStaticImpl[T: Type, D <: Idiom, N <: NamingStrategy, PrepareRow](
    quotedRaw: Expr[Quoted[Query[T]]], context: Expr[Context[D, N]]
  )(given qctx:QuoteContext, dialectTpe:TType[D], namingType:TType[N], prepareRow:TType[PrepareRow]): Expr[Option[(String, List[ScalarPlanter[_, _]])]] = {

    import qctx.tasty.{Try => TTry, _, given _}
    import io.getquill.ast.{CollectAst, QuotationTag}
    // NOTE Can disable if needed and make quoted = quotedRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val quoted = quotedRaw.unseal.underlyingArgument.seal

    def noRuntimeQuotations(ast: Ast) =
      CollectAst.byType[QuotationTag](ast).isEmpty
    
    val tryStatic =
      for {
        (idiom, naming) <- idiomAndNamingStatic
        quotedExpr <- Try {
          quoted match { // use to have 'unInline' here
            case QuotedExpr.Inline(quotedExpr) => quotedExpr
            // Warn user that a not-inline QuotedExpr was detected. Do println or is there a qctx.warn??
            case _ => println("Lifts do meet compiletime criteria"); ???
          }
        }

        lifts <- Try {
          quotedExpr.lifts match {
            case ScalarPlanterExpr.InlineList(liftsExprMatch) => liftsExprMatch
            // If lifts are not an inlineable list, throw an exception and resort to trying during runtime
            case _ => 
              println("Lifts do meet compiletime criteria:"); 
              println(quotedExpr.lifts.show)
              ???
          }
        }

        ast = new Unlifter(given qctx).apply(quotedExpr.ast)

        expandedAst <- Try { Expander.static[T](ast) } if noRuntimeQuotations(ast)
      } yield {

        // Get all existing encoders
        val encodeables = lifts.map(e => (e.uid, e)).toMap //[PrepareRow]

        val (outputAst, stmt) = idiom.translate(expandedAst)(given naming)

        val (string, externals) =
          ReifyStatement(
            idiom.liftingPlaceholder,
            idiom.emptySetContainsToken,
            stmt,
            forProbing = false
          )
        
        val encodedLifts = externals.collect {
          case tag: ScalarTag =>
            encodeables.get(tag.uid) match {
              case Some(encodeable) => encodeable
              case None => ??? 
                // TODO Throw an error here or attempt to resolve encoders during runtime?
                // maybe the user has hand-modified a quoted block and only the
                // lifts are modified with some runtime values?
                // If throwing an error (or maybe even not?) need to tell the user which lifted ids cannot be found
                // should probably add some info to the reifier to "highlight" the question marks that
                // cannot be plugged in. It would also be really nice to show the user which lift-statements
                // are wrong but that requires a bit more thought (maybe match them somehow into the original AST
                // from quotedRow via the UUID???)
            }
        }.map(_.plant)

        //val sql = stmt.toString

        println("Compile Time Query Is: " + string)

        // What about a missing decoder?
        // need to make sure that that kind of error happens during compile time
        // (also need to propagate the line number, talk to Li Houyi about that)
        '{ Option((${Expr(string)}, ${Expr.ofList(encodedLifts)})) }
      }

    if (tryStatic.isFailure) {
      println("WARNING: Dynamic Query Detected: ")
    }

    tryStatic.getOrElse {
      '{ None }
    }
  }
}




