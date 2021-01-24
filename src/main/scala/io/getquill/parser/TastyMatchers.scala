package io.getquill.parser

import scala.quoted._
import scala.quoted.Varargs

final class TastyMatchersContext(using val qctx: Quotes) extends TastyMatchers

/**
 * Allows tmporting TastyMatchers._ in order to be able to use
 * TastyMatchersContext as a matcher without having to declare it. You can just use 
 * it e.g:
 * <code>
 * import TastyMatchers._
 * foo match {
 *   case tmc.ConstantExpr(expr) => expr
 * }
 * </code>
 */
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
    println(Printer.TreeStructure.show(Untype(expr.asTerm)))

    println("================= Pretty Tree =================")
    println(pprint.apply(Untype(expr.asTerm)))
  }

  implicit class ExprOps[T: Type](expr: Expr[T]) {
    def reseal: Expr[T] = expr.asTerm.underlyingArgument.asExprOf[T]
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
          //printer.lnf(term.asTerm)
          report.throwError("Could not parse sequence expression:\n" + printer.str(term.asTerm))
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
      Untype.unapply(expr.asTerm).map(_.asExpr)

    def apply(expr: Expr[_]): Expr[_] = Untype.unapply(expr.asTerm).map(_.asExpr).get
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
      TypedMatroshkaTerm.unapply(term.asTerm).map(_.asExpr)
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

  extension (expr: Expr[_]) {
    def `.`(property: String) = {
      val method = 
        expr.asTerm.tpe.widen.classSymbol
          .getOrElse { report.throwError(s"Cannot find class symbol of the property ${expr.show}") }
          .memberMethod(property)
          .headOption
          .getOrElse { report.throwError(s"Cannot find property ${property} of ${expr.show}") }

      '{ (${ Apply(Select(expr.asTerm, method), List()).asExprOf[Any] }) }
    }
  }

  object SelectExprOpt {
    def unapply(term: Expr[_]): Option[(Expr[Option[_]], String)] = term match {
      case Unseal(Select(prefix, memberName)) => Some((prefix.asExprOf[Option[Any]], memberName))
      case _ => None
    }
  }

  object Lambda1 {
    def unapply(expr: Expr[_]): Option[(String, TypeRepr, quoted.Expr[_])] =
      unapplyTerm(expr.asTerm).map((str, tpe, expr) => (str, tpe, expr.asExpr))

    def unapplyTerm(term: Term): Option[(String, TypeRepr, Term)] = Untype(term) match {
      case Lambda(List(ValDef(ident, tpeTree, _)), methodBody) => Some((ident, tpeTree.tpe, methodBody))
      case Block(List(), expr) => unapplyTerm(expr)
      case _ => None
    }
  }

  object Lambda2 {
    def unapply(expr: Expr[_]): Option[(String, TypeRepr, String, TypeRepr, quoted.Expr[_])] =
      unapplyTerm(expr.asTerm).map((str1, tpe1, str2, tpe2, expr) => (str1, tpe1, str2, tpe2, expr.asExpr))

    def unapplyTerm(term: Term): Option[(String, TypeRepr, String, TypeRepr, Term)] = Untype(term) match {
      case Lambda(List(ValDef(ident1, tpe1, _), ValDef(ident2, tpe2, _)), methodBody) => Some((ident1, tpe1.tpe, ident2, tpe2.tpe, methodBody))
      case Block(List(), expr) => unapplyTerm(expr)
      case _ => None
    }
  }

  object RawLambdaN {
    def unapply(term: Term): Option[(List[(String, TypeRepr)], Term)] = Untype(term) match {
        case Lambda(valDefs, methodBody) => 
          val idents =
            valDefs.map {
              case ValDef(ident, typeTree, u) => (ident, typeTree.tpe)
            }

          Some((idents, methodBody))
        case Block(List(), expr) => unapply(expr)
        case _ => None
    }
  }

  object LambdaN {
    def unapply(term: Expr[_]): Option[(List[(String, TypeRepr)], quoted.Expr[_])] =
      RawLambdaN.unapply(term.asTerm).map((strAndTpe, term) => (strAndTpe, term.asExpr))
  }

  // object Lambda2 {
  //   def unapply(term: Expr[_]): Option[(String, String, quoted.Expr[_])] = term match {
  //     case Unseal(Lambda(List(ValDef(ident, _, _), ValDef(ident2, _, _)), Seal(methodBody))) => Some((ident, ident2, methodBody))
  //     case _ => None
  //   }
  // }

  object Unseal {
    def unapply(t: Expr[Any]): Option[Term] = Some(t.asTerm)
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
    val ctxTerm = ctx.asTerm
    val ctxClass = ctxTerm.tpe.widen.classSymbol.get
    ctxClass.declaredMethods.filter(f => f.name == name).headOption.getOrElse {
      throw new IllegalArgumentException(s"Cannot find method '${name}' from context ${ctx.asTerm.tpe.widen}")
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
        case Literal(StringConstant(v: String)) => Some(v)
        case Literal(IntConstant(v: Int)) => Some(v)
        case Literal(LongConstant(v: Long)) => Some(v)
        case Literal(BooleanConstant(v: Boolean)) => Some(v)
        case Literal(FloatConstant(v: Float)) => Some(v)
        case Literal(DoubleConstant(v: Double)) => Some(v)
        case Literal(ByteConstant(v: Byte)) => Some(v)
        case _ => None

  object ClassSymbol:
    def unapply(expr: Expr[Any]): Option[Symbol] =
      expr.asTerm.tpe.classSymbol

  object ClassSymbolAndUnseal:
    def unapply(expr: Expr[Any]): Option[(Symbol, Term)] =
      expr.asTerm.tpe.classSymbol.map(sym => (sym, expr.asTerm))

  /**
    * Matches `case class Person(first: String, last: String)` creation of the forms:
    *   Person("Joe","Bloggs")
    *   new Person("Joe","Bloggs")
    */
  object CaseClassCreation:
    // For modules, the _ in Select coule be a couple of things (say the class is Person):
    //   New(TypeIdent("Person$")), "<init>"), Nil) - When the case class is declared in a function body
    //   Select(This(This(Some(outerClass))), name) - When the case class is declared in the same class as the context (currently happens in actions, see the "macro" test in ActionTest.scala)
    //   Ident("Person")                            - When the case class is declared in an object or top-level
    object ModuleCreation:
      def unapply(term: Term) = term match
        case Apply(Select(New(TypeIdent(moduleType)), "<init>"), list) if (list.length == 0) && moduleType.endsWith("$") => true
        case Select(This(outerClass), name)  => true
        case Ident(name) => true
        case _ => false

    def unapply(expr: Expr[Any]): Option[(String, List[String], List[Expr[Any]])] = {
      // lazy val tpe = expr.asTerm.tpe
      // lazy val companionClass = tpe.classSymbol.get.companionClass
      // lazy val name = tpe.classSymbol.get.name
      // lazy val fields = tpe.classSymbol.get.caseFields.map(_.name) // Don't actually evaluate them unless it matches
      //println(s"@@@@@@@@@@@@@@ ***************** TRYING CASE CLASS CREATE ***************** @@@@@@@@@@@@@@\n" + Printer.TreeStructure.show(expr.asTerm))

      //def companionIsProduct(classSymbol: Symbol) = expr.asTerm.tpe.select(classSymbol.companionClass) <:< TypeRepr.of[Product]
      val out =
      UntypeExpr(expr) match
        // case Unseal(theExpr @ Apply(Select(foo, "apply"), list)) if (foo.show.contains("Contact")) =>
          // println("**************** STOP HERE ****************")
          // println(Printer.TreeStructure.show(theExpr))
          // println("Type: " + tpe)
          // println("Type Simple: " + tpe.simplified)
          // println("Selected: " + tpe.select(tpe.classSymbol.get.companionClass))
          // println("Is Product: " + isType[Product](expr))
          // println("Module: " + tpe.classSymbol.get.moduleClass)
          // println("Companion: " + tpe.classSymbol.get.companionClass)
          // println("Companion is Product: " + (tpe.select(tpe.classSymbol.get.companionClass) <:< TypeRepr.of[Product]) )
          // println("Is Module: " + ((tpe.classSymbol.get.flags & Flags.Artifact) == Flags.Artifact))
          // println("Is Module2: " + (tpe.classSymbol.get.flags.is(Flags.Artifact)))
          // println("Flags: " + (tpe.classSymbol.get.flags.show))
          // report.throwError("**************** STOP HERE ****************")
        case ClassSymbolAndUnseal(sym, Apply(Select(New(TypeIdent(_)), "<init>"), args)) if isType[Product](expr) =>
          //println("@@@@@@@@@@@@============== !!!!! MATCH ON IN-FUNC !!!!! ==============@@@@@@@@@@@@")
          Some((sym.name, sym.caseFields.map(_.name), args.map(_.asExpr)))
        case ClassSymbolAndUnseal(sym, Apply(Select(ModuleCreation(), "apply"), args)) if isType[Product](expr) => //&& sym.flags.is(Flags.Case)
          //println("@@@@@@@@@@@@============== !!!!! MATCH ON MOD !!!!! ==============@@@@@@@@@@@@")
          Some((sym.name, sym.caseFields.map(_.name), args.map(_.asExpr)))
        case _ => 
          //println("@@@@@@@@@@@@============== No Match ==============@@@@@@@@@@@@")
          None

      //println("@@@@@@@@@@@@============== OUT ==============@@@@@@@@@@@@\n" + out)
      out
    }

  // TODO Change to 'is'
  def isType[T: Type](input: Expr[_]) =
    input.asTerm.tpe <:< TypeRepr.of[T] // (implicit Type[T])

  // TODO Change to 'are'
  def is[T: Type](inputs: Expr[_]*): Boolean =
    inputs.forall(input => input.asTerm.tpe <:< TypeRepr.of[T])
}