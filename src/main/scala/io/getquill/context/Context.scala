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
import io.getquill.ast.Ast
import scala.quoted.{Type => TType, _}
import miniquill.quoter.FindLifts
import io.getquill.idiom.Idiom

import io.getquill._

// TODO Non Portable
trait Context[Dialect <: io.getquill.idiom.Idiom, Naming <: io.getquill.NamingStrategy] 
extends EncodingDsl
//  extends Closeable
//  with CoreDsl 
{
  implicit inline def autoDecoder[T]:Decoder[T] = GenericDecoder.derived

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

  // TODO Need to have some implicits to auto-convert stuff inside
  // of the run function itself into a quotation?

  inline def expandAst[T](quoted: Quoted[Query[T]]):(Ast, Tuple) = {
    val (ast, lifts) = (quoted.ast, quoted.lifts)

    println("Before Expansion: " + ast)
    val expandedAst = Expander.runtime[T](ast)
    println("After Expansion: " + expandedAst)

    (expandedAst, lifts)
  }

  inline def runDynamic[T](quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val (expandedAst, lifts) = expandAst(quoted)
    val (outputAst, stmt) = idiom.translate(expandedAst)(given naming)
    val queryString = stmt.toString
    // summon a decoder and a expander (as well as an encoder) all three should be provided by the context
    val decoder =
      summonFrom {
        // TODO Implicit summoning error
        case decoder: Decoder[T] => decoder
      }
    val extractor = (r: ResultRow) => decoder.apply(1, r)
    this.executeQuery(queryString, extractor)
  }

  inline def run[T](quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {

    val staticQuery = translateStatic[T](quoted)
    if (staticQuery.isDefined) { // TODO Find a way to generate the static query or not and then check (somehow use an optional there?)

      val decoder =
        summonFrom {
          case decoder: Decoder[T] => decoder
        }
      val extractor = (r: ResultRow) => decoder.apply(1, r)
      this.executeQuery(staticQuery.get, extractor)

    } else {
      runDynamic(quoted)
    }
  }

  // todo add 'prepare' i.e. encoders here
  def executeQuery[T](sql: String, extractor: Extractor[T]): Result[RunQueryResult[T]]

  protected val identityPrepare: Prepare = (Nil, _)
  protected val identityExtractor = identity[ResultRow] _

  inline def translateStatic[T](inline quoted: Quoted[Query[T]]): Option[String] =
    ${ Context.translateStaticImpl[T, Dialect, Naming]('quoted, 'this) }
}

object Context {
  import miniquill.parser._
  import scala.quoted._ // summonExpr is actually from here
  import scala.quoted.matching._ // ... or from here

  // TODO Pick these up implicitly in the macro?
  def parserFactory: (QuoteContext) => PartialFunction[Expr[_], Ast] = 
    (qctx: QuoteContext) => new Parser(given qctx)

  def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: QuoteContext) => new Lifter(given qctx)  

  import io.getquill.idiom.LoadNaming
  import io.getquill.util.LoadObject

  def idiomAndNamingStatic[D<:io.getquill.idiom.Idiom, N<:io.getquill.NamingStrategy](given qctx: QuoteContext, dialectTpe:TType[D], namingType:TType[N]): Try[(Idiom, NamingStrategy)] =
    for {
      idiom <- LoadObject(dialectTpe)
      namingStrategy <- LoadNaming.static(namingType)
    } yield (idiom, namingStrategy)

  // TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
  def translateStaticImpl[T: Type, D <: Idiom, N <: NamingStrategy](quoted: Expr[Quoted[Query[T]]], context: Expr[Context[D, N]])(given qctx:QuoteContext, dialectTpe:TType[D], namingType:TType[N]): Expr[Option[String]] = {
    val qctx = implicitly[QuoteContext]
    import qctx.tasty.{Try => TTry, _, given _}
    import io.getquill.ast.{CollectAst, QuotationTag}


    def noRuntimeQuotations(ast: Ast) =
      CollectAst.byType[QuotationTag](ast).isEmpty
    
    val tryStatic =
      for {
        (idiom, naming) <- idiomAndNamingStatic
        ast        = parserFactory(qctx).apply(quoted)
        reifiedAst = lifterFactory(qctx)(ast)
        expandedAst <- Try(Expander.static[T](ast)) if noRuntimeQuotations(ast)
      } yield {
        
        val (outputAst, stmt) = idiom.translate(expandedAst)(given naming)
        val sql = stmt.toString

        println("Compile Time Query Is: " + sql)

        // What about a missing decoder?
        // need to make sure that that kind of error happens during compile time
        // (also need to propagate the line number, talk to Li Houyi about that)
        '{ Some(${Expr(sql)}) }
      }

    tryStatic.getOrElse('{ None })
  }
}




