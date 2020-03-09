package miniquill.quoter

import scala.quoted._
import scala.quoted.matching._
import miniquill.dsl.GenericEncoder
import io.getquill.ast.Ast
import miniquill.parser.MatroshkaHelper
import miniquill.parser.SealUnseal
import miniquill.quoter.Quoted


case class ScalarPlanterExpr(uid: String, expr: Expr[Any], encoder: Expr[GenericEncoder[Any, Any]]) {
  // TODO Change to 'replant' ?
  // Plant the ScalarPlanter back into the Scala AST
  def plant(given qctx: QuoteContext) = {
    '{ ScalarPlanter($expr, $encoder, ${Expr(uid)}) }
  }
}
object ScalarPlanterExpr {
  object Inline {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[ScalarPlanterExpr] = expr match {
      case vase @ '{ ScalarPlanter.apply[$qt, $prep]($liftValue, $encoder, ${scala.quoted.matching.Const(uid: String)}) } =>
        Some(ScalarPlanterExpr(uid, vase, encoder.asInstanceOf[Expr[GenericEncoder[Any, Any]]]))
      case _ => 
        None
    }
  }

  protected object `(ScalarPlanter).unquote` {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[Expr[ScalarPlanter[Any, Any]]] = expr match {
      case '{ ($scalarPlanter: ScalarPlanter[$tt, $pr]).unquote } => 
        Some(scalarPlanter.asInstanceOf[Expr[ScalarPlanter[Any, Any]]])
      case _ => 
        None
    }
  }

  object InlineUnquote {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[ScalarPlanterExpr] = expr match {
      case `(ScalarPlanter).unquote`(planterUnquote) =>
        planterUnquote match {
          case Inline(planterExpr) => 
            Some(planterExpr)
          case _ => 
            // All lifts re-inserted as ScalarPlanters must be inlined values containing
            // their UID as well as a corresponding tree. An error should be thrown if this is not the case.
            qctx.throwError("Format of ScalarLift holder must be fully inline.", expr)
        }
      case _ => None
    }
  }


  def findUnquotes(expr: Expr[Any])(given qctx: QuoteContext): List[ScalarPlanterExpr] =
    ExprAccumulate(expr) {
      case InlineUnquote(scalarPlanter) => scalarPlanter
    }

  // TODO Find a way to propogate PrepareRow into here
  // pull vases out of Quotation.lifts
  object InlineList {
    def unapply(expr: Expr[List[Any]])(given qctx: QuoteContext): Option[List[ScalarPlanterExpr]] = expr match {
      case '{ scala.List[$t](${ExprSeq(elems)}: _*) } => 
        val scalarValues = 
          elems.collect {
            case ScalarPlanterExpr.Inline(vaseExpr) => vaseExpr
          }

        // if all the elements match SingleValueVase then return them, otherwise don't
        if (scalarValues.length == elems.length) Some(scalarValues.toList)
        else None

      case _ => None
    }
  }
}

case class QuotedExpr(ast: Expr[Ast], lifts: Expr[List[ScalarPlanter[Any, Any]]], runtimeQuotes: Expr[List[QuotationVase]])
object QuotedExpr {
    //object `EmptyQuotationPouchList`

  // Note, the quotation is not considered to be inline if there are any runtime lifts
  object Inline {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[QuotedExpr] = {
      import qctx.tasty.{Term => QTerm, given, _}
      val matroshkaHelper = new MatroshkaHelper
      import matroshkaHelper._
    
      expr match {
        /* No runtime lifts allowed for inline quotes so quotationPouches.length must be 0 */

        // TODO TODO listArgsApply needs to be an empty list
        case '{ Quoted.apply[$qt]($ast, $v, $listArgsApply) } => 
          Some(QuotedExpr(ast, v, listArgsApply))
        case 
          TypedMatroshka(tree) => Inline.unapply(tree)
        case _ => 
          None
      }
    }
  }
}


sealed trait QuotationBinExpr
object QuotationBinExpr {

  protected object `(QuotationBin).unquote` {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext) = expr match {
      // When a QuotationBin is embedded into an ast
      case '{ (${quotationBin}: QuotationBin[$tt]).unquote } => Some(quotationBin)
      case _ => None
    }
  }

  protected object `QuotationBin.apply` {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[(Expr[Quoted[Any]], String)] = expr match {
      case '{ QuotationBin.apply[$qt]($quotation, ${scala.quoted.matching.Const(uid: String)}) } => 
        Some((quotation, uid))
      case _ => None
    }
  }

  object findUnquotes {
    def apply(expr: Expr[Any])(given qctx: QuoteContext) =
      ExprAccumulate(expr) {
        case InlineOrPluckedUnquoted(vaseExpr) => vaseExpr
      }
  }

  // Doesn't look like this is needed
  // object InlineUnquoted {
  //   def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[InlineableQuotationBinExpr] = {
  //     InlineOrPlucked match {
  //       case inlineable: InlineableQuotationBinExpr => Some(inlineable)
  //       case _ => None
  //     }
  //   }
  // }

  object InlineOrPluckedUnquoted {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[QuotationBinExpr] = expr match {
      case `(QuotationBin).unquote`(QuotationBinExpr.InlineOrPlucked(vaseExpr)) => Some(vaseExpr)
      case _ => None
    }
  }

  // Verify that a quotation is inline. It is inline if all the lifts are inline. There is no need
  // to search the AST since it has been parsed already
  object InlineOrPlucked {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[QuotationBinExpr] = {
      expr match {
        case vase @ `QuotationBin.apply`(QuotedExpr.Inline(ast, ScalarPlanterExpr.InlineList(lifts), _), uid) => // TODO Also match .unapply?
          Some(InlineableQuotationBinExpr(uid, ast, vase.asInstanceOf[Expr[QuotationBin[Any]]], lifts))

        case `QuotationBin.apply`(quotation, uid) =>
          Some(PluckedQuotationBinExpr(uid, quotation))

        // If it's a QuotationBin but we can't extract it at all, need to throw an error
        case '{ ($qb: QuotationBin[$t]) } =>
          qctx.throwError("Invalid quotation form. Quotations need to at least contain the inline block needed to extract a UID.", qb)

        case _ => 
          None
      }
    }
  }
}


// QuotationBins that have runtime values hance cannot be re-planted into the scala AST and
// they need to be put into QuotationVasees
case class PluckedQuotationBinExpr(uid: String, expr: Expr[Quoted[Any]]) extends QuotationBinExpr {
  def toVaseExpr(given qctx: QuoteContext) = '{ QuotationVase($expr, ${Expr(uid)}) }
}

// QuotationBins expressions that can be further inlined into quotated clauses
case class InlineableQuotationBinExpr(
  uid: String, 
  ast: Expr[Ast],
  vase: Expr[QuotationBin[Any]], 
  inlineLifts: List[ScalarPlanterExpr]
) extends QuotationBinExpr