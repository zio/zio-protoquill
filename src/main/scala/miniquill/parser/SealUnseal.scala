package miniquill.parser

import scala.quoted._

class TastyMatchersContext(given val qctx: QuoteContext) extends TastyMatchers

trait TastyMatchers {
  implicit val qctx: QuoteContext
  import qctx.tasty.{given, _}

  object SelectApply1 {
    def unapply(term: Expr[_]): Option[(Expr[_], String, Expr[_])] = term match {
      case Unseal(Apply(Select(body, method), List(arg))) => Some((body.seal, method, arg.seal))
      case Unseal(Apply(TypeApply(Select(body, method), _), List(arg))) => Some((body.seal, method, arg.seal))
      case _ => None
    }
  }

  object TypedMatroshka {
    // need to define a case where it won't go into matcher otherwise recursion is infinite
    //@tailcall // should be tail recursive
    def recurse(innerTerm: Term): Term = innerTerm match {
      case Typed(innerTree, _) => recurse(innerTree)
      case other => other
    }

    def unapply(term: Expr[Any]): Option[Expr[Any]] = term.unseal match {
      case Typed(tree, _) => Some(recurse(tree).seal)
      case _ => None
    }
  }

  object SelectExpr {
    def unapply(term: Expr[_]): Option[(Expr[_], String)] = term match {
      case Unseal(Select(Seal(prefix), memberName)) => Some((prefix, memberName))
      case _ => None
    }
  }

  object `.` {
    def unapply(term: Expr[_]): Option[(Expr[_], String)] = term match {
      case Unseal(Select(Seal(prefix), memberName)) => Some((prefix, memberName))
      case _ => None
    }
  }

  object SelectExprOpt {
    def unapply(term: Expr[_]): Option[(Expr[Option[_]], String)] = term match {
      case Unseal(Select(prefix, memberName)) => Some((prefix.seal.cast[Option[Any]], memberName))
      case _ => None
    }
  }

  object Lambda1 {
    def unapply(term: Expr[_]): Option[(String, quoted.Expr[_])] = term match {
      case Unseal(Lambda(List(ValDef(ident, _, _)), Seal(methodBody))) => Some((ident, methodBody))
      case _ => None
    }
  }

  object Lambda2 {
    def unapply(term: Expr[_]): Option[(String, String, quoted.Expr[_])] = term match {
      case Unseal(Lambda(List(ValDef(ident, _, _), ValDef(ident2, _, _)), Seal(methodBody))) => Some((ident, ident2, methodBody))
      case _ => None
    }
  }

  object Unseal {
    def unapply(t: Expr[Any]) = {
      Some(t.unseal)
    }
  }
  object Seal {
    def unapply[T](e: Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.seal.asInstanceOf[quoted.Type[T]]
      Some(e.seal.cast[T])
    }
  }
}