package miniquill.parser

import io.getquill.ast.{Ident => Idnt, Constant => Const, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.quoted.matching._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable

class QuotationParser(given qctx: QuoteContext) {
  import qctx.tasty.{_, given _}
  
  // TODO Refactor out into a trait?
  private object Unseal {
    def unapply(t: Expr[Any]) = {
      Some(t.unseal)
    }
  }

  import qctx.tasty.{_, given _}
  import scala.quoted.matching.{Const => Constant} //hello

  private object TypedMatroshka {
    // need to define a case where it won't go into matcher otherwise recursion is infinite
    //@tailcall // should be tail recursive
    def recurse(innerTerm: Term): Term = innerTerm match {
      case Typed(innerTree, _) => recurse(innerTree)
      case other => other
    }

    def unapply(term: Term): Option[Term] = term match {
      case Typed(tree, _) => Some(recurse(tree))
      case _ => None
    }
  }

  object `Quoted.apply` {
    def unapply(expr: Expr[Any]): Option[Expr[Ast]] = expr match {
      case '{ Quoted.apply[$qt]($ast, $v) } => 
        //println("********************** MATCHED VASE INNER TREE **********************")
        //printer.lnf(expr.unseal)
        Some(ast)
      case Unseal(TypedMatroshka(tree)) => unapply(tree.seal)
      case _ => 
        //println("********************** NOT MATCHED VASE INNER TREE **********************")
        //printer.lnf(expr.unseal)
        None
    }
  }

  protected object `QuotationVase.apply` {
    def unapply(expr: Expr[Any]) = expr match {
      case vase @ '{ QuotationVase.apply[$qt]($quotation, ${scala.quoted.matching.Const(uid: String)}) } => 
        //println("********************** MATCHED VASE APPLY **********************")
        //printer.lnf(expr.unseal)
        Some((quotation, uid, vase))
      case _ => None
    }
  }

  // Match the QuotationVase(...).unquote values which are tacked on to every
  // child-quote (inside of a parent quote) when the 'unquote' function (i.e macro)
  // is applied.
  protected object `QuotationVase.unquote` {
    def unapply(expr: Expr[Any]) = expr match {
      // When a QuotationVase is embedded into an ast
      case '{ (${quotationVase}: QuotationVase[$tt]).unquote } => Some(quotationVase)
      case _ => None
    }
  }

  object MatchRuntimeQuotation {
    def unapply(expr: Expr[Any]): Option[(Expr[Any], String)] =
      expr match {
        // case MatchQuotationRef(tree, uuid) => 
        //   println("******************** Runtime: Match Quotation Ref ********************")
        //   printer.lnf((tree.unseal, uuid))
        //   Some((tree, uuid))
        case `QuotationVase.unquote`(innards) =>
          //println("******************** Runtime: Match Unquote ********************")
          //printer.lnf(innards.unseal)
          unapply(innards)
        // sometimes there are multiple levels of vases when references are spliced,
        // we should only care about the innermost one
        case `QuotationVase.apply`(_, uuid, vase) =>
          //println("******************** Runtime: Vase Apply ********************")
          //printer.lnf(uuid, vase)
          Some((vase, uuid))
        case _ => None
      }
    }

  object MatchInlineQuotation {
    def unapply(expr: Expr[Any]): Option[(Expr[Ast], String)] =
      expr match {
        case `QuotationVase.unquote`(`QuotationVase.apply`(`Quoted.apply`(astTree), uuid, _)) =>
          Some((astTree, uuid))
        case _ => None
      }
  }
}
