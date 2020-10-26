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
import scala.quoted.{Type => TType, _}
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

    inline def translateStatic[T](inline quoted: Quoted[Query[T]]): Option[(String, List[ScalarPlanter[_, _]])] =
      ${ StaticTranslationMacro[T, Dialect, Naming]('quoted) }
  
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
  
      println("==================<<<<<<<<<<< GOT TO HERE >>>>>>>>>>>>>>>>================")
      this.executeQuery(queryString, prepare, extractor, ExecutionType.Static)
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
        //null.asInstanceOf[Result[RunQueryResult[T]]]

      case None =>
        
        // NOTE The second these are commented out, the compilation and run seems to work
        // it seems like something in the runDynamic is causing the failure
        val decoder = summonDecoder[R]
        runDynamic[R, T](quoted, decoder, converter)
        
        throw new IllegalArgumentException("(((((((((((((( NO STATIC CONTEXT ))))))))))))))))))")
    }
  }
  

  inline def runQuery[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val staticState = translateStatic[T](quoted)
    println("&&&&&&&&&&&&&&&&&&&&&&&&&&& WAS FROM FROM HERE &&&&&&&&&&&&&&&&&&&&&&&&&")
    println("&&&&&&&&&&&&&&&&&&&&&&&&&&& WAS FROM FROM HERE &&&&&&&&&&&&&&&&&&&&&&&&&")
    println("&&&&&&&&&&&&&&&&&&&&&&&&&&& WAS FROM FROM HERE &&&&&&&&&&&&&&&&&&&&&&&&&")
    println("&&&&&&&&&&&&&&&&&&&&&&&&&&& WAS FROM FROM HERE &&&&&&&&&&&&&&&&&&&&&&&&&")
    println("&&&&&&&&&&&&&&&&&&&&&&&&&&& WAS FROM FROM HERE &&&&&&&&&&&&&&&&&&&&&&&&&")
    // KNOW I GO HERE. WOW, SUMMONFROM IS ODD!!!!

    val r = encodeAndExecute[T, T](staticState, quoted, t => t)
    r
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
