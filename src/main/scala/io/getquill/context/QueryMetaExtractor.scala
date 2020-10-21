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

import io.getquill._

/**
* A QueryMeta allows contra-mapping some Query[T] to a combination of a Query[R] and then
* an extractor R => T. That is to say a function Query[T] => Query[R] and R => T function
* is automatically swapped in for a Query[T].
*
* Internally, we use the term 'quip' (i.e. query + flip) to mean the QueryMeta construct,
* The Query[T] => Query[R] function itself is called the Quipper.
* Since a QueryMeta comes with an R=>M contramap 
* function to apply to an extractor we call that the 'baq' since it mapps the inner query back
* from R to T.
*
* Once the quip is summoned, it is applied to the original user-create query and then called
* a requip (i.e. re-applied quip). That it to say the requip is:
* `FunctionApply(Query[T] => Query[R], Query[R])`
* 
* Note that since internally, a QueryMeta carries a Quoted instance, the QueryMeta itself
* is a QuotationLot. For that reason, we call the whole QueryMeta structure a quip-lot.
* (Metaphorically speaking, a 'lot' meta real-estate containing a quip)
*/
object QueryMetaExtractor {
  import miniquill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import miniquill.quoter.ScalarPlanter
  import miniquill.quoter._
  import io.getquill.ast.FunctionApply

  inline def apply[T, R, D <: io.getquill.idiom.Idiom, N <: io.getquill.NamingStrategy](
    inline quotedRaw: Quoted[Query[T]],
    inline ctx: Context[D, N]
  ): (Quoted[Query[R]], R => T, Option[(String, List[ScalarPlanter[_, _]])]) = 
    ${ applyImpl[T, R, D, N]('quotedRaw, 'ctx) }


  def summonQueryMeta[T: Type, R: Type](using qctx:QuoteContext): Option[Expr[QueryMeta[T, R]]] =
    Expr.summon(using '[QueryMeta[T, R]])

  case class StaticRequip[T, R](requip: Expr[Quoted[Query[R]]], baq: Expr[R => T])

  def attemptStaticRequip[T: Type, R: Type](
    queryLot: QuotedExpr, 
    queryLifts: List[ScalarPlanterExpr[_, _]], 
    quip: Expr[QueryMeta[T, R]]
  )(using qctx: QuoteContext): Option[StaticRequip[T, R]] = {
    
    val quipLotExpr = quip match {
      case QuotationLotExpr(qbin) => qbin
      case _ => Reporting.throwError("QueryMeta expression is not in a valid form: " + quip)
    }
    
    quipLotExpr match {
      // todo try astMappingFunc rename to `Ast(T => r)` or $r
      case Uprootable(uid, quipperAst, _, quotation, lifts, List(baq)) => 
        // Don't need to unlift the ASTs and re-lift them. Just put them into a FunctionApply
        val astApply = 
          '{FunctionApply($quipperAst, List(${queryLot.ast}))}

        // TODO Dedupe?
        val newLifts = (lifts ++ queryLifts).map(_.plant)

        // In the compile-time case, we can synthesize the new quotation
        // much more easily since we can just combine the lifts and Apply the
        // QueryMeta quotation to the initial one
        
        // Syntheize a new quotation to combine the lifts and quotes of the reapplied
        // query. I do not want to use QuoteMacro here because that requires a parser
        // which means that the Context will require a parser as well. That will
        // make the parser harder to customize by users
        val reappliedQuery =
          '{ Quoted[Query[R]]($astApply, ${Expr.ofList(newLifts)}, Nil) } // has to be strictly Nil otherwise does not match

        val extractorFunc = '{ $baq.asInstanceOf[R => T] }

        Some(StaticRequip(reappliedQuery, extractorFunc))
        
      // In these two cases, the quoation applies during runtime at which point the quotation of the quip
      // and query quotes and lifts will all be done during runtime.
      case _: Pluckable | _: Pointable => 
        None
    }
  }

  def applyImpl[T: Type, R: Type, D <: io.getquill.idiom.Idiom: Type, N <: io.getquill.NamingStrategy: Type](
    quotedRaw: Expr[Quoted[Query[T]]],
    ctx: Expr[Context[D, N]]
  )(using qctx:QuoteContext): Expr[(Quoted[Query[R]], R => T, Option[(String, List[ScalarPlanter[_,_]])])] = {
    import qctx.tasty.{Try => TTry, _}
    val quotedArg = quotedRaw.unseal.underlyingArgument.seal.cast[Quoted[Query[T]]]
    val summonedMeta = Expr.summon(using '[QueryMeta[T, R]]).map(_.unseal.underlyingArgument.seal.cast[QueryMeta[T, R]])
    summonedMeta match {
      case Some(quip) =>
        val possiblyUprootableQuery = QuotedExpr.uprootableWithLiftsOpt(quotedArg)

        possiblyUprootableQuery match {
          case Some((queryLot, queryLifts)) =>
            attemptStaticRequip[T, R](queryLot, queryLifts, quip) match {
              
              case Some(StaticRequip(requip, baq)) =>
                val staticTranslation = StaticTranslationMacro[R, D, N](requip)
                '{ ($requip, $baq, $staticTranslation) }

              case None =>
                println("WARNING: Query Was Static but Dynamic Meta was found which forced the query to become dynamic!")

                val reappliedAst = 
                  '{ FunctionApply($quip.entity.ast, List($quotedArg.ast)) }
    
                val requip =
                  '{ Quoted[Query[R]]($reappliedAst, $quip.entity.lifts ++ $quotedArg.lifts, $quip.entity.runtimeQuotes ++ $quotedArg.runtimeQuotes) }
    
                '{ ($requip, $quip.extract, None) }
            }

          case None =>
            val reappliedAst = 
              '{ FunctionApply($quip.entity.ast, List($quotedArg.ast)) }

            val requip =
              '{ Quoted[Query[R]]($reappliedAst, $quip.entity.lifts ++ $quotedArg.lifts, $quip.entity.runtimeQuotes ++ $quotedArg.runtimeQuotes) }

            '{ ($requip, $quip.extract, None) }
        }
        
        //Reporting.throwError("Quote Meta Identified but not found!")
      case None => 
        Reporting.throwError("Quote Meta needed but not found!")
    }
  }
}
