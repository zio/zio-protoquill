package miniquill.parser

import scala.quoted._
import scala.quoted.Varargs

final class TastyMatchersContext(using val qctx: Quotes) extends TastyMatchers

object TastyMatchers {
  inline def tmc(using Quotes) = new TastyMatchersContext
}

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

  /** Summon a named method from the context Context[D, N] */
  def summonContextMethod(name: String, ctx: Expr[_]) = {
    val ctxTerm = Term.of(ctx)
    val ctxClass = ctxTerm.tpe.widen.classSymbol.get
    ctxClass.methods.filter(f => f.name == name).headOption.getOrElse {
      throw new IllegalArgumentException(s"Cannot find method '${name}' from context ${Term.of(ctx).tpe.widen}")
    }
  }

  object ConstantValue:
    type Kind = String | Char | Int | Long | Boolean | Float | Double | Byte
    def unapply(any: Any): Option[Kind] =
      any match {
        case _: String  => Some(any.asInstanceOf[Kind])
        case _: Char  => Some(any.asInstanceOf[Kind])
        case _: Int  => Some(any.asInstanceOf[Kind])
        case _: Long  => Some(any.asInstanceOf[Kind])
        case _: Boolean  => Some(any.asInstanceOf[Kind])
        case _: Float  => Some(any.asInstanceOf[Kind])
        case _: Double  => Some(any.asInstanceOf[Kind])
        case _: Byte => Some(any.asInstanceOf[Kind])
        case _ => None
      }

  object ConstantExpr:
    // def Any(v: Any): Expr[Any] =
    //   v match 
    //     case cv: String | Char | Int | Long | Boolean | Float | Double | Byte => apply(cv)
        // case _ => report.throwError(s"Cannot lift constant value: ${v}, it is not one of the allowed constant types: String | Int | Long | Boolean | Float | Double | Byte")

    def apply[T <: ConstantValue.Kind](const: T): Expr[T]  =
      const match
        case v: String => Expr(v)
        case v: Char => Expr(v)
        case v: Int => Expr(v)
        case v: Long => Expr(v)
        case v: Boolean => Expr(v)
        case v: Float => Expr(v)
        case v: Double => Expr(v)
        case v: Byte => Expr(v)

    def unapply[T <: ConstantValue.Kind](t: Expr[T]) =
      t match
        case Const(v) => Some(v)
        case _ => None
        

  object ConstantTerm:
    def unapply(term: Term): Option[ConstantValue.Kind] =
      term match
        case Literal(Constant.String(v: String)) => Some(v)
        case Literal(Constant.Int(v: Int)) => Some(v)
        case Literal(Constant.Long(v: Long)) => Some(v)
        case Literal(Constant.Boolean(v: Boolean)) => Some(v)
        case Literal(Constant.Float(v: Float)) => Some(v)
        case Literal(Constant.Double(v: Double)) => Some(v)
        case Literal(Constant.Byte(v: Byte)) => Some(v)
        case _ => None
}