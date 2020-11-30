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
import scala.quoted._
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

  inline def runQuery[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] = 
    ${ RunDsl.runQueryImpl[T, ResultRow, PrepareRow, Dialect, Naming, Result[RunQueryResult[T]]]('quoted, 'this) }
}

object StaticExtractor {
  def apply[PrepareRowTemp](lifts: List[ScalarPlanter[_, _]], row: PrepareRowTemp) = {
    val (_, values, prepare) =
      lifts.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val newRow = 
            lift
            .asInstanceOf[ScalarPlanter[Any, PrepareRowTemp]]
            .encoder(idx, lift.value, row).asInstanceOf[PrepareRowTemp] // TODO since summoned encoders are casted
          (idx + 1, lift.value :: values, newRow)
      }
    (values, prepare)
  }
}



object RunDsl {

  import io.getquill.idiom.{ Idiom => Id }
  import io.getquill.{ NamingStrategy => Na }

  // =========================== Trying This Approach Instead ========================
  def runQueryImpl[T: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    quoted: Expr[Quoted[Query[T]]],
    ctx: Expr[Context[D, N]]
  )(using qctx: Quotes): Expr[Res] = 
  {
    import qctx.reflect._

    val ctxTerm = Term.of(ctx)
    val ctxClass = ctxTerm.tpe.widen.classSymbol.get
    //println("================= Found Fields: =============\n" + ctxClass.methods.map(_.name).mkString("\n"))
    val executeQuery = ctxClass.methods.filter(f => f.name == "executeQuery").head

    // Asking Stucki how to summon the query meta
    val tmc = new miniquill.parser.TastyMatchersContext
    import tmc._

    Expr.summon[QueryMeta[T, _]] match {
      case Some(expr) =>
        println("Summoned! " + expr.show)
        UntypeExpr(expr) match {
          case '{ QueryMeta.apply[k, n]($one, $two, $uid) } => println(s"GOT TYPE: ${TypeRepr.of[n].show}") 
          case _ => println("DID NOT GET TYPE")
        }

      case None => "bar"
    }

    val staticStateOpt = StaticTranslationMacro.applyInner[T, D, N](quoted)

    val output =
    staticStateOpt match {
      case Some((query, lifts)) =>


        // TODO if there's  a type that gets summoned the T here is is that someR
        Expr.summon[GenericDecoder[ResultRow, T]] match {
          case Some(decoder) =>
            val extractor = '{ (r: ResultRow) => $decoder.apply(1, r) }
            val prepare = '{ (row: PrepareRow) => StaticExtractor.apply[PrepareRow]($lifts, row) }

            val applyExecuteQuery =
              Apply(
                TypeApply(
                  Select(ctxTerm, executeQuery),
                  List(TypeTree.of[T])
                ),
                List(Term.of(Expr(query)), Term.of('{null}), Term.of(extractor), Term.of('{ExecutionType.Static}))
              )
            val res = applyExecuteQuery.asExprOf[Res]
            res

            // =================== This Doesn't work ==================
            //val output = '{ $ctx.executeQuery(${Expr(query)}, null, $extractor, ExecutionType.Static) }

          case None =>
            report.throwError("Decoder could not be summoned")
        }

  //       // val decoder =
  //       //   summonFrom {
  //       //     case decoder: GenericDecoder[ResultRow, R] => decoder
  //       //     // TODO Extract into meta function, have a good error if decoder not found
  //       //   }
  //       val decoder = summonDecoder[R]
  //       runStatic[R, T](query, lifts, decoder, converter)
  //       //null.asInstanceOf[Result[RunQueryResult[T]]]


      case None => 
        report.throwError("Could not summon static context")
    }

    output
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
