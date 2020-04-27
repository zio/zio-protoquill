package miniquill.parser

import scala.quoted._
import scala.quoted.matching._

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

  // Designed to be a more generic version the ExprSeq which does not handle all cases.
  // Particularily when a varargs parameter is passed from one inline function into another.
  object GenericSeq {
    def unapply(term: Expr[_]): Option[List[Expr[_]]] = {
      term match {
        case ExprSeq(props) => Some(props.toList)
        case TypedMatroshka(Unseal(Repeated(props, _))) => Some(props.map(_.seal))
      }
    }
  }

  object TypedMatroshka {
    // need to define a case where it won't go into matcher otherwise recursion is infinite
    //@tailcall // should be tail recursive
    def recurse(innerTerm: Term): Term = innerTerm match {
      case Typed(innerTree, _) => recurse(innerTree)
      case other => other
    }

    def unapplyTerm(term: Term): Option[Term] = term match {
      case Typed(tree, _) => Some(recurse(tree))
      case other => Some(other)
    }

    def unapply(term: Expr[Any]): Option[Expr[Any]] = term.unseal match {
      case Typed(tree, _) => Some(recurse(tree).seal)
      case other => Some(other.seal)
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
    def unapplyTerm(term: Term): Option[(String, Term)] = term match {
      case Lambda(List(ValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
      case Block(List(), expr) => unapplyTerm(expr)
      case _ => None
    }

    def unapply(term: Expr[_]): Option[(String, quoted.Expr[_])] =
      unapplyTerm(term.unseal).map((str, term) => (str, term.seal))
  }

  object Lambda2 {
    def unapply(term: Expr[_]): Option[(String, String, quoted.Expr[_])] = term match {
      case Unseal(Lambda(List(ValDef(ident, _, _), ValDef(ident2, _, _)), Seal(methodBody))) => Some((ident, ident2, methodBody))
      case _ => None
    }
  }

  object Unseal {
    def unapply(t: Expr[Any]): Option[Term] = {
      TypedMatroshka.unapplyTerm(t.unseal)
    }
  }
  object Seal {
    def unapply[T](e: Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.seal.asInstanceOf[quoted.Type[T]]
      Some(e.seal.cast[T])
    }
  }
}