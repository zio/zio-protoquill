package io.getquill.context

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
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import scala.quoted._
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill._
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.quat.Quat

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
 * Once the quip is summoned, it is applied to the original user-created query and then called
 * a requip (i.e. re-applied quip). That it to say the requip is:
 * `FunctionApply(Query[T] => Query[R], Query[R])`
 *
 * Note that since internally, a QueryMeta carries a Quoted instance, the QueryMeta itself
 * is a QuotationLot. For that reason, we call the whole QueryMeta structure a quip-lot.
 * (Metaphorically speaking, a 'lot' meta real-estate containing a quip)
 *
 * Given a PersonName(name: String) we can define a QueryMeta like this:
 * {{
 *   inline given QueryMeta[PersonName, String] =
 *    queryMeta(
 *      quote { (q: Query[PersonName]) => q.map(p => p.name) } // The Quipper
 *    )((name: String) => PersonName(name)) // The Baq
 * }}
 * When we do something like:
 * {{
 *   inline def people = quote { query[PersonName] }
 *   val result = ctx.run(people)
 * }}
 * The Query-Lot AST becomes EntityQuery("Person")
 */
object QueryMetaExtractor {
  import io.getquill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import io.getquill.Planter
  import io.getquill.metaprog.QuotationLotExpr
  import io.getquill.metaprog.Uprootable
  import io.getquill.metaprog.Pluckable
  import io.getquill.metaprog.Pointable
  import io.getquill.ast.FunctionApply

  // inline def apply[T, R, D <: io.getquill.idiom.Idiom, N <: io.getquill.NamingStrategy](
  //   inline quotedRaw: Quoted[Query[T]],
  //   inline ctx: Context[D, N]
  // ): (Quoted[Query[R]], R => T, Option[(String, List[Planter[_, _]])]) =
  //   ${ applyImpl[T, R, D, N]('quotedRaw, 'ctx) }

  def summonQueryMeta[T: Type, R: Type](using Quotes): Option[Expr[QueryMeta[T, R]]] =
    Expr.summon[QueryMeta[T, R]]

  case class StaticRequip[T, R](requip: Expr[Quoted[Query[R]]], baq: Expr[R => T])

  def attemptStaticRequip[T: Type, R: Type](
      queryLot: QuotedExpr,
      queryLifts: List[PlanterExpr[_, _, _]],
      quip: Expr[QueryMeta[T, R]]
  )(using Quotes): Option[StaticRequip[T, R]] = {
    import quotes.reflect.report

    val quipLotExpr = quip match {
      case QuotationLotExpr(qbin) => qbin
      case _                      => report.throwError("QueryMeta expression is not in a valid form: " + quip)
    }

    quipLotExpr match {
      // todo try astMappingFunc rename to `Ast(T => r)` or $r
      case up @ Uprootable(uid, quipperAst, lifts) =>
        val baq =
          up.extra match {
            case List(baq) => baq
            case _         => report.throwError("Invalid Query Meta Form. ContraMap 'baq' function not defined.")
          }

        // Don't need to unlift the ASTs and re-lift them. Just put them into a FunctionApply
        // import io.getquill.util.Format
        // import io.getquill.util.Messages.qprint
        // println(s"=== Quipper Ast: ${(Unlifter(quipperAst))}")
        // println(s"=== Query Lot Ast: ${(Unlifter(queryLot.ast))}")

        // E.g. apply quipper ast: `(q) => q.map(p => p.name)` to queryLot.ast which is querySchema("PersonName") (i.e. EntityQuery("PersonName"))
        // that will become `querySchema("PersonName").map(p => p.name)`
        val astApply =
          '{ FunctionApply($quipperAst, List(${ queryLot.ast })) }

        // TODO Dedupe the lifts?
        val newLifts = (queryLifts ++ lifts).map(_.plant)

        // In the compile-time case, we can synthesize the new quotation
        // much more easily since we can just combine the lifts and Apply the
        // QueryMeta quotation to the initial one

        // Synthesize a new quotation to combine the lifts and quotes of the reapplied
        // query. I do not want to use QuoteMacro here because that requires a parser
        // which means that the Context will require a parser as well. That will
        // make the parser harder to customize by users
        val reappliedQuery =
          '{ Quoted[Query[R]]($astApply, ${ Expr.ofList(newLifts) }, Nil) } // has to be strictly Nil otherwise does not match

        val extractorFunc = '{ $baq.asInstanceOf[R => T] }

        Some(StaticRequip(reappliedQuery, extractorFunc))

      // In these two cases, the quotation applies during runtime at which point the quotation of the quip
      // and query quotes and lifts will all be done during runtime.
      case _: Pluckable | _: Pointable =>
        None
    }
  }

  def applyImpl[T: Type, R: Type, D <: io.getquill.idiom.Idiom: Type, N <: io.getquill.NamingStrategy: Type](
      quotedRaw: Expr[Quoted[Query[T]]],
      topLevelQuat: Quat
  )(using Quotes): (Expr[Quoted[Query[R]]], Expr[R => T], Option[StaticState]) = {
    import quotes.reflect.{Try => TTry, _}
    val quotedArg = quotedRaw.asTerm.underlyingArgument.asExprOf[Quoted[Query[T]]]
    val summonedMeta = Expr.summon[QueryMeta[T, R]].map(x => x.asTerm.underlyingArgument.asExprOf[QueryMeta[T, R]])
    summonedMeta match {
      case Some(quip) =>
        val possiblyUprootableQuery = QuotedExpr.uprootableWithLiftsOpt(quotedArg)

        possiblyUprootableQuery match {
          case Some((queryLot, queryLifts)) =>
            attemptStaticRequip[T, R](queryLot, queryLifts, quip) match {

              case Some(StaticRequip(requip, baq)) =>
                val staticTranslation = StaticTranslationMacro[D, N](requip, ElaborationBehavior.Elaborate, topLevelQuat)
                (requip, baq, staticTranslation)

              case None =>
                report.warning(s"Query Was Static but a Dynamic Meta was found: `${io.getquill.util.Format.Expr(quip)}`.This has forced the query to become dynamic!")

                val reappliedAst =
                  '{ FunctionApply($quip.entity.ast, List($quotedArg.ast)) }

                val requip =
                  '{ Quoted[Query[R]]($reappliedAst, $quip.entity.lifts ++ $quotedArg.lifts, $quip.entity.runtimeQuotes ++ $quotedArg.runtimeQuotes) }

                (requip, '{ $quip.extract }, None)
            }

          case None =>
            val reappliedAst =
              '{ FunctionApply($quip.entity.ast, List($quotedArg.ast)) }

            val requip =
              '{ Quoted[Query[R]]($reappliedAst, $quip.entity.lifts ++ $quotedArg.lifts, $quip.entity.runtimeQuotes ++ $quotedArg.runtimeQuotes) }

            (requip, '{ $quip.extract }, None)
        }

      // report.throwError("Quote Meta Identified but not found!")
      case None =>
        report.throwError("Quote Meta needed but not found!")
    }
  }
}
