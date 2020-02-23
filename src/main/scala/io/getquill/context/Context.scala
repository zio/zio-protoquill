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
import miniquill.quoter.ScalarValueVase
import io.getquill.derived._
import miniquill.context.mirror.MirrorDecoders
import miniquill.context.mirror.Row
import miniquill.dsl.GenericDecoder
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import scala.quoted.{Type => TType, _}
import miniquill.quoter.FindLifts
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationVase

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

  // inline def expandLifts[T](inline quoted: Quoted[T]): List[ScalarValueVase[_]] = {
  //   val lifts = quoted.lifts match {
  //     case sl: ScalarValueVase[_] => sl
  //   }
  // }

  inline def runDynamic[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val ast = Expander.runtime[T](quoted.ast)
    val lifts = 
      if (quoted.lifts.isInstanceOf[Product])
        quoted.lifts.asInstanceOf[Product].productIterator.toList
      else
        List()

    val quotationVases = 
      lifts.collect {
        case v: QuotationVase[Any] => v // Not sure why but QuotationVase[_] causes errors
      }

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
    val decoder =
      summonFrom {
        // TODO Implicit summoning error
        case decoder: Decoder[T] => decoder
      }
    val extractor = (r: ResultRow) => decoder.apply(1, r)
    this.executeQuery(queryString, null, extractor, ExecutionType.Dynamic)
  }

  inline def run[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val staticQuery = translateStatic[T](quoted)
    if (staticQuery.isDefined) { // TODO Find a way to generate the static query or not and then check (somehow use an optional there?)

      val decoder =
        summonFrom {
          case decoder: Decoder[T] => decoder
        }
      val extractor = (r: ResultRow) => decoder.apply(1, r)
      this.executeQuery(staticQuery.get, null, extractor, ExecutionType.Static)

    } else {
      runDynamic(quoted)
    }
  }

  // todo add 'prepare' i.e. encoders here
  def executeQuery[T](sql: String, prepare: Prepare, extractor: Extractor[T], executionType: ExecutionType): Result[RunQueryResult[T]]

  protected val identityPrepare: Prepare = (Nil, _)
  protected val identityExtractor = identity[ResultRow] _

  inline def translateStatic[T](inline quoted: Quoted[Query[T]]): Option[String] =
    ${ Context.translateStaticImpl[T, Dialect, Naming]('quoted, 'this) }
}

object Context {
  import miniquill.parser._
  import scala.quoted._ // summonExpr is actually from here
  import scala.quoted.matching._ // ... or from here

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

  def idiomAndNamingStatic[D<:io.getquill.idiom.Idiom, N<:io.getquill.NamingStrategy](given qctx: QuoteContext, dialectTpe:TType[D], namingType:TType[N]): Try[(Idiom, NamingStrategy)] =
    for {
      idiom <- LoadObject(dialectTpe)
      namingStrategy <- LoadNaming.static(namingType)
    } yield (idiom, namingStrategy)

  // TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
  def translateStaticImpl[T: Type, D <: Idiom, N <: NamingStrategy](quotedRaw: Expr[Quoted[Query[T]]], context: Expr[Context[D, N]])(given qctx:QuoteContext, dialectTpe:TType[D], namingType:TType[N]): Expr[Option[String]] = {
    import qctx.tasty.{Try => TTry, _, given _}
    import io.getquill.ast.{CollectAst, QuotationTag}
    // NOTE Can disable if needed and make quoted = quotedRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val quoted = quotedRaw.unseal.underlyingArgument.seal

    val quotationParser = new QuotationParser
    import quotationParser._

    def noRuntimeQuotations(ast: Ast) =
      CollectAst.byType[QuotationTag](ast).isEmpty
    
    val tryStatic =
      for {
        (idiom, naming) <- idiomAndNamingStatic
        // We only need an unlifter here, not a parser (**)
        // TODO Need to pull out lifted sections from the AST to process lifts
        ast = {
          object Unseal {
            def unapply(t: Expr[Any]) = {
              Some(t.unseal)
            }
          }
          def unInline(expr: Expr[Any]): Ast = 
            expr match {
              // Need to traverse through this case if we want to be able to use inline parameter value
              // without having to do quoted.unseal.underlyingArgument.seal
              case Unseal(Inlined(_, _, v)) => unInline(v.seal)
              case `Quoted.apply`(ast) =>
                new Unlifter(given qctx).apply(ast)
            }

          unInline(quoted)
          // TODO If not found, fail the macro. Make sure user knows about it!
          // TODO Test this out, an easy way is to make the match invalid
          // for example by changing 'quoted.unseal.underlyingArgument.seal' to just 'quoted'
        }
        expandedAst <- Try { 
          Expander.static[T](ast) 
        } if noRuntimeQuotations(ast)
      } yield {

        val (outputAst, stmt) = idiom.translate(expandedAst)(given naming)
        val sql = stmt.toString

        println("Compile Time Query Is: " + sql)

        // What about a missing decoder?
        // need to make sure that that kind of error happens during compile time
        // (also need to propagate the line number, talk to Li Houyi about that)
        '{ Some(${Expr(sql)}) }
      }

    if (tryStatic.isFailure) {
      println("WARNING: Dynamic Query Detected")
    }

    tryStatic.getOrElse {
      '{ None }
    }
  }
}




