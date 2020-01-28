package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.dsl.CoreDsl
//import io.getquill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ NamingStrategy, ReturnAction }
import miniquill.quoter.Query

import miniquill.dsl.EncodingDsl
import miniquill.quoter.Quoted
import miniquill.quoter.Query
import miniquill.context.Context
import io.getquill.idiom.Idiom
import io.getquill.derived._
import miniquill.context.mirror.MirrorDecoders
import miniquill.context.mirror.Row
import miniquill.dsl.GenericDecoder

// TODO Non Portable
trait Context[Dialect <: Idiom, Naming <: NamingStrategy] 
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

  def idiom: Idiom
  def naming: Naming

  // TODO Need to have some implicits to auto-convert stuff inside
  // of the run function itself into a quotation.

  inline def run[T](quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = {
    val (ast, lifts) = (quoted.ast, quoted.lifts)
    
    val expander =
      summonFrom {
        // TODO Implicit summoning error
        case expander: Expander[T] => expander
      }

    println("Before Expansion: " + ast)
    val expandedAst = expander.expandAst(ast)
    println("After Expansion: " + expandedAst)

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

  //inline def run[T](quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = ???
  // todo add 'prepare' i.e. encoders here
  def executeQuery[T](sql: String, extractor: Extractor[T]): Result[RunQueryResult[T]]

  //def run[T](quoted: Quoted[T]): Result[RunQuerySingleResult[T]] = macro QueryMacro.runQuerySingle[T]
  
  // def run[T](quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = macro QueryMacro.runQuery[T]



  // def prepare[T](quoted: Quoted[Query[T]]): Session => Result[PrepareRow] = macro QueryMacro.prepareQuery[T]

  // def run(quoted: Quoted[Action[_]]): Result[RunActionResult] = macro ActionMacro.runAction
  // def run[T](quoted: Quoted[ActionReturning[_, T]]): Result[RunActionReturningResult[T]] = macro ActionMacro.runActionReturning[T]
  // def run(quoted: Quoted[BatchAction[Action[_]]]): Result[RunBatchActionResult] = macro ActionMacro.runBatchAction
  // def run[T](quoted: Quoted[BatchAction[ActionReturning[_, T]]]): Result[RunBatchActionReturningResult[T]] = macro ActionMacro.runBatchActionReturning[T]
  // def prepare(quoted: Quoted[Action[_]]): Session => Result[PrepareRow] = macro ActionMacro.prepareAction
  // def prepare(quoted: Quoted[BatchAction[Action[_]]]): Session => Result[List[PrepareRow]] = macro ActionMacro.prepareBatchAction

  protected val identityPrepare: Prepare = (Nil, _)
  protected val identityExtractor = identity[ResultRow] _

  // protected def handleSingleResult[T](list: List[T]) =
  //   list match {
  //     case value :: Nil => value
  //     case other        => fail(s"Expected a single result but got $other")
  //   }
}




