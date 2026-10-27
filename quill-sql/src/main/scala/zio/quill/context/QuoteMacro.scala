package zio.quill.context

import scala.quoted._
import zio.quill.parser.ParserFactory
import zio.quill.util.Load
import zio.quill.norm.BetaReduction
import zio.quill.parser.engine.Parser
import zio.quill.parser.Lifter
import zio.quill.metaprog.PlanterExpr
import zio.quill.metaprog.QuotationLotExpr
import zio.quill.metaprog.Pluckable
import zio.quill.metaprog.Pointable
import zio.quill.Quoted
import zio.quill.metaprog.SummonParser
import zio.quill.metaprog.SummonSerializationBehaviors
import zio.quill.parser.engine.History
import zio.quill.context.sql.norm.SimplifyFilterTrue
import zio.quill.parser.Unlifter
import zio.quill.util.Format
import zio.quill.norm.TranspileConfig
import zio.quill.metaprog.SummonTranspileConfig
import zio.quill.ast.Ast
import zio.quill.ast.StatefulTransformer
import zio.quill.QuotationVase

object ExtractLifts {
  // Find all lifts, dedupe by UID since lifts can be inlined multiple times hence
  // appearing in the AST in multiple places.
  def extractLifts(body: Expr[Any])(using Quotes) = {
    // TODO If we want to support functionality to resolve all LazyPlanters into
    // eager Planters before the 'run' function, need to look thorugh eager/lazy
    // (planters) with the same UID and choose the ones that are eager for the same UID
    // i.e. since they would be the resolved ones
    // val m =

    // order of the lifts shuold not matter
    // PlanterExpr.findUnquotes(body).zipWithIndex           // preserve the original order
    //   .groupBy((r, idx) => r.uid)                         // group by uids
    //   .map((uid, planters) => planters.sortBy(_._2).head) // for each uid, pick first index
    //   .toList.sortBy(_._2).map(_._1)                      // once that's done, sort by original index
    //   .map(_.plant)                                       // then replant

    import quotes.reflect._

    // TODO First one for each UID should win because it's the outermost one, should make sure tha't the case
    // PlanterExpr.findUnquotes(body).foldLeft(LinkedHashMap[String, PlanterExpr[_, _, _]]())((elem, map) => (map.addIfNotContains(elem.uid, elem)))
    PlanterExpr.findUnquotes(body).distinctBy(_.uid).map(_.plant)
  }

  def extractRuntimeUnquotes(body: Expr[Any])(using Quotes) = {
    import quotes.reflect.report
    val unquotes = QuotationLotExpr.findUnquotes(body)
    unquotes
      .collect {
        case expr: Pluckable => expr
        case Pointable(expr) =>
          report.throwError(s"Invalid runtime Quotation: ${expr.show}. Cannot extract a unique identifier.", expr)
      }
      .distinctBy(_.uid)
      .map(_.pluck)
  }

  def apply(body: Expr[Any])(using Quotes) =
    (extractLifts(body), extractRuntimeUnquotes(body))
}

object QuoteMacro {

  object DynamicsExtractor {
    import zio.quill.ast.Dynamic
    import zio.quill.ast.QuotationTag
    import java.util.UUID
    import zio.quill.metaprog.Extractors._

    case class Extractee(uid: String, dynamic: Dynamic)
    case class Transform(state: List[Extractee]) extends StatefulTransformer[List[Extractee]] {
      override def apply(e: Ast): (Ast, StatefulTransformer[List[Extractee]]) =
        e match {
          case dyn: Dynamic =>
            val uid = UUID.randomUUID().toString()
            (QuotationTag(uid), Transform(Extractee(uid, dyn) +: state))
          case _ => super.apply(e)
        }

    }

    def apply(ast: Ast)(using Quotes): (Ast, List[Expr[QuotationVase]]) = {
      import quotes.reflect._

      def printAstWithCustom(ast: Ast)(uid: String, replacementText: String) = {
        import zio.quill.MirrorIdiom._
        import zio.quill.idiom.StatementInterpolator._
        import zio.quill.ast.External
        implicit def externalTokenizer: Tokenizer[External] =
          Tokenizer[External] {
            case QuotationTag(tagUid) if (tagUid == uid) =>
              replacementText.token
            case _ => stmt"?"
          }
        ast.token.toString
      }

      val (newAst, transformer) = Transform(List())(ast)
      val extracted = transformer.state.reverse
      val quotations =
        extracted.map {
          case Extractee(uid, Dynamic(value, quat)) =>
            val quotation =
              value match {
                case expr: Expr[_] if (is[Quoted[_]](expr)) =>
                  expr.asExprOf[Quoted[_]]
                case expr: Expr[_] =>
                  report.throwError(s"Dynamic value has invalid expression: ${Format.Expr(expr)} in the AST:\n${printAstWithCustom(newAst)(uid, "<INVALID-HERE>")}")
                case other =>
                  report.throwError(s"Dynamic value is not an expression: ${other} in the AST:\n${printAstWithCustom(newAst)(uid, "<INVALID-HERE>")}")
              }

            '{ QuotationVase($quotation, ${ Expr(uid) }) }
        }

      (newAst, quotations)
    }
  }

  def apply[T](bodyRaw: Expr[T])(using Quotes, Type[T], Type[Parser]): Expr[Quoted[T]] = {
    import quotes.reflect._

    // NOTE Can disable underlyingArgument here if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = bodyRaw.asTerm.underlyingArgument.asExpr

    val parser = SummonParser().assemble
    val (serializeQuats, serializeAst) = SummonSerializationBehaviors()
    given TranspileConfig = SummonTranspileConfig()

    val rawAst = parser(body)
    val (noDynamicsAst, dynamicQuotes) = DynamicsExtractor(rawAst)
    val ast = SimplifyFilterTrue(BetaReduction(noDynamicsAst))

    val reifiedAst = Lifter.WithBehavior(serializeQuats, serializeAst)(ast)
    val u = Unlifter(reifiedAst)

    // Extract runtime quotes and lifts
    val (lifts, pluckedUnquotes) = ExtractLifts(bodyRaw)
    '{ Quoted[T](${ reifiedAst }, ${ Expr.ofList(lifts) }, ${ Expr.ofList(pluckedUnquotes ++ dynamicQuotes) }) }
  }
}
