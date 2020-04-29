package miniquill.quoter

import scala.quoted._
import scala.quoted.matching._
import miniquill.dsl.GenericEncoder
import io.getquill.ast.Ast
import miniquill.parser.TastyMatchersContext
import miniquill.parser.TastyMatchersContext
import miniquill.quoter.Quoted

/* As the different kinds of parsing in Quill-Dotty became more complex, the need for an
overarching model of "how stuff works" became necessary. There are several places in the
Quill codebase where Dotty trees need to be parsed and their contents extracted, the most
noteable is the parser. However, a second important place is the methods that extract
lifts and quotations. This model is for the latter.

The conceptual model for this context is the following. A Dotty (i.e. Tasty) tree
is extracted from a quotation (a.k.a. "the ground") into either a Planter or a Vase.
When the tree matches a couple of criteria, it is re-insertable into the next quotation
and therefore placed into a Planter. Otherwise, the tree is *not* re-insertable and it 
has to be "plucked" and inserted into a vase. Until we ascertain whether to re-insert
or pluck, the Tree is held (temporarily) inside of a Bin.

Different construcuts follow these rules in different ways. Scalar values for instances
cannot contain contents making them non-re-insertable and therefore are always
held inside of planters (i.e. the ScalarPlanter) and replanted back into the part
of the tree constructing the PrepareRow in the 'run' method. 
Quotations are held in a QuotationBin until it is determined 
whether they are re-insertable. If they are, the Parser
will transparently read the AST through them. Otherwise they will be 'plucked' into a
QuotationVase and eventually processed during runtime.
*/
class ExprModel {
}


// Holds and parses variations of the ScalarPlanter
case class ScalarPlanterExpr[T: Type, PrepareRow: Type](uid: String, expr: Expr[T], encoder: Expr[GenericEncoder[T, PrepareRow]]) {
  // TODO Change to 'replant' ?
  // Plant the ScalarPlanter back into the Scala AST
  def plant(given qctx: QuoteContext) = {
    '{ ScalarPlanter[T, PrepareRow]($expr, $encoder, ${Expr(uid)}) }
  }
}
object ScalarPlanterExpr {
  object Inline {
    

    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[ScalarPlanterExpr[_, _]] = 
      expr match {
        case '{ ScalarPlanter.apply[$qt, $prep]($liftValue, $encoder, ${scala.quoted.matching.Const(uid: String)}) } =>
          Some(ScalarPlanterExpr(uid, liftValue, encoder/* .asInstanceOf[Expr[GenericEncoder[A, A]]] */))
        case _ => 
          None
      }
  }

  protected object `(ScalarPlanter).unquote` {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[Expr[ScalarPlanter[_, _]]] = expr match {
      case '{ ($scalarPlanter: ScalarPlanter[$tt, $pr]).unquote } => 
        Some(scalarPlanter/* .asInstanceOf[Expr[ScalarPlanter[A, A]]] */)
      case _ => 
        None
    }
  }

  object InlineUnquote {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[ScalarPlanterExpr[_, _]] = expr match {
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


  def findUnquotes(expr: Expr[Any])(given qctx: QuoteContext): List[ScalarPlanterExpr[_, _]] =
    ExprAccumulate(expr) {
      case InlineUnquote(scalarPlanter) => scalarPlanter
    }

  // TODO Find a way to propogate PrepareRow into here
  // pull vases out of Quotation.lifts
  object InlineList {
    def unapply(expr: Expr[List[Any]])(given qctx: QuoteContext): Option[List[ScalarPlanterExpr[_, _]]] = {
      expr match {
        case '{ Nil } =>
          Some(List())

        case '{ scala.List.apply[$t](${ExprSeq(elems)}: _*) } => 
          val scalarValues = 
            elems.collect {
              case ScalarPlanterExpr.Inline(vaseExpr) => vaseExpr
            }

          import qctx.tasty.{given, _}
          println("****************** GOT HERE **************")
          println(s"Scalar values: ${scalarValues.mkString("(", ",", ")")}")
          println(s"Elems: ${elems.map(_.show).mkString("(", ",", ")")}")

          // if all the elements match SingleValueVase then return them, otherwise don't
          if (scalarValues.length == elems.length) Some(scalarValues.toList)
          else None

        case _ => None
      }
    }
  }
}

case class QuotedExpr(ast: Expr[Ast], lifts: Expr[List[ScalarPlanter[_, _]]], runtimeQuotes: Expr[List[QuotationVase]])
object QuotedExpr {
    //object `EmptyQuotationPouchList`

  // Note, the quotation is not considered to be inline if there are any runtime lifts
  object Inline {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[QuotedExpr] = {
      import qctx.tasty.{Term => QTerm, given, _}
      val tmc = new TastyMatchersContext
      import tmc._

      expr match {
        /* No runtime lifts allowed for inline quotes so quotationPouches.length must be 0 */
        case exprr @  '{ Quoted.apply[$qt]($ast, $v, Nil) } =>  //List.apply[$ttt](${ExprSeq(args)}: _*) if (args.length == 0)
          Some(QuotedExpr(ast, v, '{ List[QuotationVase]() }))
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
      // There are situations e.g. SchemaMeta where there's an additional type ascription needed
      // there it needs to be specified in the AST manually. Maybe this is a bug?
      case '{ type $tt; ((${quotationBin}: QuotationBin[`$tt`]).unquote: `$tt`) } => Some(quotationBin)
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
        case InlineOrPluckedUnquoted(vaseExpr) => 
          vaseExpr
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
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[QuotationBinExpr] = 
      expr match {
        case `(QuotationBin).unquote`(QuotationBinExpr.InlineOrPlucked(vaseExpr)) => Some(vaseExpr)
        case _ => None
      }
  }

  // Verify that a quotation is inline. It is inline if all the lifts are inline. There is no need
  // to search the AST since it has been parsed already
  object InlineOrPlucked {
    def unapply(expr: Expr[Any])(given qctx: QuoteContext): Option[QuotationBinExpr] = {
      import qctx.tasty.{given, _}

      
      expr match {
        case vase @ `QuotationBin.apply`(QuotedExpr.Inline(ast, ScalarPlanterExpr.InlineList(lifts), _), uid) => // TODO Also match .unapply?
          Some(InlineableQuotationBinExpr(uid, ast, vase.asInstanceOf[Expr[QuotationBin[Any]]], lifts))

        case `QuotationBin.apply`(quotation, uid) =>
          Some(PluckableQuotationBinExpr(uid, quotation))

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
case class PluckableQuotationBinExpr(uid: String, expr: Expr[Quoted[Any]]) extends QuotationBinExpr {
  def pluck(given qctx: QuoteContext) = '{ QuotationVase($expr, ${Expr(uid)}) }
}

// QuotationBins expressions that can be further inlined into quotated clauses
case class InlineableQuotationBinExpr(
  uid: String, 
  ast: Expr[Ast],
  vase: Expr[QuotationBin[Any]], 
  inlineLifts: List[ScalarPlanterExpr[_, _]]
) extends QuotationBinExpr