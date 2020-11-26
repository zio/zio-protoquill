package miniquill.parser

import scala.quoted._
import scala.quoted.Varargs

class TastyMatchersContext(using val qctx: Quotes) extends TastyMatchers

trait TastyMatchers {
  implicit val qctx: Quotes
  import qctx.reflect._

  def printExpr(expr: Expr[_], label: String = "") = {
    if (label != "")
      println(s"--------------------------------- ${label} ---------------------------------")

    println("================= Tree =================")
    println(expr.show)

    println("================= Matchers =================")
    println(Untype(Term.of(expr)).showExtractors)

    println("================= Pretty Tree =================")
    println(pprint.apply(Untype(Term.of(expr))))
  }

  implicit class ExprOps[T: Type](expr: Expr[T]) {
    def reseal: Expr[T] = Term.of(expr).underlyingArgument.asExprOf[T]
  }

  object SelectApply1 {
    def unapply(term: Expr[_]): Option[(Expr[_], String, Expr[_])] = term match {
      case Unseal(Apply(Select(body, method), List(arg))) => Some((body.asExpr, method, arg.asExpr))
      case Unseal(Apply(TypeApply(Select(body, method), _), List(arg))) => Some((body.asExpr, method, arg.asExpr))
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
        case Unseal(Untype(Repeated(props, _))) => Some(props.map(_.asExpr))
        case other =>
          //println("Could not parse sequence expression:")
          //printer.lnf(Term.of(term))
          report.throwError("Could not parse sequence expression:\n" + printer.str(Term.of(term)))
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
      Untype.unapply(Term.of(expr)).map(_.asExpr)

    def apply(expr: Expr[_]): Expr[_] = Untype.unapply(Term.of(expr)).map(_.asExpr).get
  }

  // Always match (whether ast starts with Typed or not). If it does, strip the Typed node.
  object Untype {
    def unapply(term: Term): Option[Term] = term match {
      case TypedMatroshkaTerm(t) => Some(t)
      case other => Some(other)
    }

    def apply(term: Term) = Untype.unapply(term).get
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
      TypedMatroshkaTerm.unapply(Term.of(term)).map(_.asExpr)
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
      case Unseal(Select(prefix, memberName)) => Some((prefix.asExprOf[Option[Any]], memberName))
      case _ => None
    }
  }

  object Lambda1 {
    def unapply(expr: Expr[_]): Option[(String, quoted.Expr[_])] =
      unapplyTerm(Term.of(expr)).map((str, expr) => (str, expr.asExpr))

    def unapplyTerm(term: Term): Option[(String, Term)] = Untype(term) match {
      case Lambda(List(ValDef(ident, _, _)), methodBody) => Some((ident, methodBody))
      case Block(List(), expr) => unapplyTerm(expr)
      case _ => None
    }
  }

  object Lambda2 {
    def unapply(expr: Expr[_]): Option[(String, String, quoted.Expr[_])] =
      unapplyTerm(Term.of(expr)).map((str1, str2, expr) => (str1, str2, expr.asExpr))

    def unapplyTerm(term: Term): Option[(String, String, Term)] = Untype(term) match {
      case Lambda(List(ValDef(ident1, _, _), ValDef(ident2, _, _)), methodBody) => Some((ident1, ident2, methodBody))
      case Block(List(), expr) => unapplyTerm(expr)
      case _ => None
    }
  }

  object RawLambdaN {
    def unapply(term: Term): Option[(List[String], Term)] = Untype(term) match {
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
      RawLambdaN.unapply(Term.of(term)).map((str, term) => (str, term.asExpr))
  }

  // object Lambda2 {
  //   def unapply(term: Expr[_]): Option[(String, String, quoted.Expr[_])] = term match {
  //     case Unseal(Lambda(List(ValDef(ident, _, _), ValDef(ident2, _, _)), Seal(methodBody))) => Some((ident, ident2, methodBody))
  //     case _ => None
  //   }
  // }

  object Unseal {
    def unapply(t: Expr[Any]): Option[Term] = Some(Term.of(t))
  }
  object Seal {
    def apply[T](e: Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.asType.asInstanceOf[quoted.Type[T]]
      e.asExprOf[T]
    }

    def unapply[T](e: Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.asType.asInstanceOf[quoted.Type[T]]
      Some(e.asExprOf[T])
    }
  }

  object TupleName {
    def unapply(str: String): Boolean = str.matches("Tuple[0-9]+")
  }
  object TupleIdent {
    def unapply(term: Term): Boolean =
      term match {
        case Ident(TupleName()) => true
        case _ => false
      }
  }

    object UntypeApply {
    private def recurse(term: Term): Term = {
      //println("============== Recursing UntypeApply =============")
      term match {
        case TypeApply(content, args) => recurse(content)
        case other => other
      }
    }
    def unapply(term: Term) = Some(recurse(term))
  }

  object Method0 {
    def unapply(term: Term): Option[(Term, String)] =
      term match {
        case UntypeApply(Select(source, methodName)) => Some((source, methodName))
        case _ => None
      }
  }

  object UntypeTree {
    def recurse(innerTerm: Tree): Tree = innerTerm match {
      case Typed(innerTree, _) => recurse(innerTree)
      case other => other
    }

    def unapply(term: Tree): Option[Tree] = Some(recurse(term))
    def apply(term: Tree) = UntypeTree.unapply(term).get
  }
}