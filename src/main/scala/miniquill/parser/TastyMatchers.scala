package miniquill.parser

import scala.quoted._
import scala.quoted.Varargs

class TastyMatchersContext(using val qctx: QuoteContext) extends TastyMatchers

trait TastyMatchers {
  implicit val qctx: QuoteContext
  import qctx.reflect.{TypeRepr => TType, _}

  implicit class ExprOps[T: Type](expr: Expr[T]) {
    def reseal: Expr[T] = expr.unseal.underlyingArgument.seal.cast[T]
  }

  object SelectApply1 {
    def unapply(term: Expr[_]): Option[(Expr[_], String, Expr[_])] = term match {
      case Unseal(Apply(Select(body, method), List(arg))) => Some((body.seal, method, arg.seal))
      case Unseal(Apply(TypeApply(Select(body, method), _), List(arg))) => Some((body.seal, method, arg.seal))
      case _ => None
    }
  }

  // Designed to be a more generic version the Varargs which does not handle all cases.
  // Particularily when a varargs parameter is passed from one inline function into another.
  object GenericSeq {
    def unapply(term: Expr[_]): Option[List[Expr[_]]] = {
      term match {
        case Varargs(props) => Some(props.toList)
        case '{ List(${Varargs(props)}) } => Some(props.toList)
        case '{ Nil } => Some(List())
        case '{ Seq(${Varargs(props)}) } => Some(props.toList)
        case Unseal(Untype(Repeated(props, _))) => Some(props.map(_.seal))
        case other =>
          //println("Could not parse sequence expression:")
          //printer.lnf(term.unseal)
          report.throwError("Could not parse sequence expression:\n" + printer.str(term.unseal))
      }
    }
  }

  // Whether you are unapplying this or applying this to an Expr, the functionality is the same.
  // Take a Expr that has a type ascription (e.g. '{ blah: BlahType }) and remove the type ascription
  // if such an ascription exists (note that there could be more then one e.g.
  // '{ ((blah: BlahType): BlahType) } ). If there are no type ascriptions, just return the term.
  // The unapply allows it to be done inside of a matcher.
  object UntypeExpr {
    def unapply(expr: Expr[_]): Option[Expr[_]] = 
      Untype.unapply(expr.unseal).map(_.seal)

    def apply(expr: Expr[_]): Expr[_] = Untype.unapply(expr.unseal).map(_.seal).get
  }

  // Always match (whether ast starts with Typed or not). If it does, strip the Typed node.
  object Untype {
    def unapply(term: Term): Option[Term] = term match {
      case TypedMatroshkaTerm(t) => Some(t)
      case other => Some(other)
    }
  }

  object TypedMatroshkaTerm {
    def recurse(innerTerm: Term): Term = innerTerm match {
      case Typed(innerTree, _) => recurse(innerTree)
      case other => other
    }

    def unapply(term: Term): Option[Term] = term match {
      case Typed(tree, _) => Some(recurse(tree))
      case other => None
    }
  }

  object TypedMatroshka {
    def unapply(term: Expr[Any]): Option[Expr[Any]] = 
      TypedMatroshkaTerm.unapply(term.unseal).map(_.seal)
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

  object RawLambdaN {
    def unapply(term: Term): Option[(List[String], Term)] = term match {
        case Lambda(valDefs, methodBody) => 
          val idents =
            valDefs.map {
              case ValDef(ident, i, u) => ident
            }

          Some((idents, methodBody))
        case Block(List(), expr) => unapply(expr)
        case _ => None
    }
  }

  object LambdaN {
    def unapply(term: Expr[_]): Option[(List[String], quoted.Expr[_])] =
      RawLambdaN.unapply(term.unseal).map((str, term) => (str, term.seal))
  }

  object Lambda2 {
    def unapply(term: Expr[_]): Option[(String, String, quoted.Expr[_])] = term match {
      case Unseal(Lambda(List(ValDef(ident, _, _), ValDef(ident2, _, _)), Seal(methodBody))) => Some((ident, ident2, methodBody))
      case _ => None
    }
  }

  object Unseal {
    def unapply(t: Expr[Any]): Option[Term] = Some(t.unseal)
  }
  object Seal {
    def unapply[T](e: Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.seal.asInstanceOf[quoted.Type[T]]
      Some(e.seal.cast[T])
    }
  }
}