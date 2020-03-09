package miniquill.parser

import io.getquill.ast.{ Ident => Idnt, Constant => Const, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.quoted.matching._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable

class MatroshkaHelper(given val qctx: QuoteContext) {
  import qctx.tasty.{Term => QTerm, given, _}

  object TypedMatroshka {
    // need to define a case where it won't go into matcher otherwise recursion is infinite
    //@tailcall // should be tail recursive
    def recurse(innerTerm: QTerm): QTerm = innerTerm match {
      case Typed(innerTree, _) => recurse(innerTree)
      case other => other
    }

    def unapply(term: Expr[Any]): Option[Expr[Any]] = term.unseal match {
      case Typed(tree, _) => Some(recurse(tree).seal)
      case _ => None
    }
  }
}