package io.getquill.metaprog

import scala.quoted._
import scala.quoted.Varargs
import io.getquill.util.printer

class Is[T: Type]:
  def unapply(expr: Expr[Any])(using Quotes) =
    import quotes.reflect._
    if (expr.asTerm.tpe <:< TypeRepr.of[T])
      Some(expr)
    else
      None

object Extractors {
  def printExpr(using Quotes)(expr: Expr[_], label: String = "") = {
    import quotes.reflect._
    if (label != "")
      println(s"--------------------------------- ${label} ---------------------------------")

    println("================= Tree =================")
    println(expr.show)

    println("================= Matchers =================")
    println(Printer.TreeStructure.show(Untype(expr.asTerm)))

    println("================= Pretty Tree =================")
    println(pprint.apply(Untype(expr.asTerm)))
  }

  extension [T: Type](expr: Expr[T])
    def reseal(using Quotes): Expr[T] =
      import quotes.reflect._
      expr.asTerm.underlyingArgument.asExprOf[T]

  object SelectApply1 {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[_], String, Expr[_])] =
      import quotes.reflect._
      term match {
        case Unseal(Apply(Select(body, method), List(arg))) => Some((body.asExpr, method, arg.asExpr))
        case Unseal(Apply(TypeApply(Select(body, method), _), List(arg))) => Some((body.asExpr, method, arg.asExpr))
        case _ => None
      }
  }

  // Designed to be a more generic version the Varargs which does not handle all cases.
  // Particularily when a varargs parameter is passed from one inline function into another.
  object GenericSeq {
    def unapply(using Quotes)(term: Expr[_]): Option[List[Expr[_]]] = {
      import quotes.reflect._
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
    def unapply(using Quotes)(expr: Expr[_]): Option[Expr[_]] =
      import quotes.reflect._
      Untype.unapply(expr.asTerm).map(_.asExpr)

    def apply(using Quotes)(expr: Expr[_]): Expr[_] =
      import quotes.reflect._
      Untype.unapply(expr.asTerm).map(_.asExpr).get
  }

  // Always match (whether ast starts with Typed or not). If it does, strip the Typed node.
  object Untype {
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] = term match {
      case TypedMatroshkaTerm(t) => Some(t)
      case other => Some(other)
    }

    def apply(using Quotes)(term: quotes.reflect.Term) = Untype.unapply(term).get
  }

  object TypedMatroshkaTerm {
    def recurse(using Quotes)(innerTerm: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect._
      innerTerm match
        case Typed(innerTree, _) => recurse(innerTree)
        case other => other

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect._
      term match
        case Typed(tree, _) => Some(recurse(tree))
        case other => None
  }

  object TypedMatroshka {
    def unapply(using Quotes)(term: Expr[Any]): Option[Expr[Any]] =
      import quotes.reflect._
      TypedMatroshkaTerm.unapply(term.asTerm).map(_.asExpr)
  }

  object SelectExpr {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[_], String)] =
      import quotes.reflect._
      term match {
        case Unseal(Select(Seal(prefix), memberName)) => Some((prefix, memberName))
        case _ => None
      }
  }

  object `.` {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[_], String)] =
      import quotes.reflect._
      term match {
        case Unseal(Select(Seal(prefix), memberName)) => Some((prefix, memberName))
        case _ => None
      }
  }

  extension (expr: Expr[_]) {
    def `.`(property: String)(using Quotes) = {
      import quotes.reflect._
      val cls =
          expr.asTerm.tpe.widen.classSymbol.getOrElse {
            report.throwError(s"Cannot find class symbol of the property ${expr.show}", expr)
          }
      val method =
        cls.memberFields // using memberFields might be more efficient but with it we have no control over the error messages since if method doesn't exist, exception is thrown right away
          .find(sym => sym.name == property)
          .getOrElse {
            report.throwError(s"Cannot find property '${property}' of (${expr.show}:${cls.name}) fields are: ${cls.memberFields.map(_.name)}", expr)
          }

      '{ (${ Select(expr.asTerm, method).asExpr }) }
    }
  }

  object SelectExprOpt {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[Option[_]], String)] =
      import quotes.reflect._
      term match {
        case Unseal(Select(prefix, memberName)) => Some((prefix.asExprOf[Option[Any]], memberName))
        case _ => None
      }
  }

  object Lambda1 {
    def unapply(using Quotes)(expr: Expr[_]): Option[(String, quotes.reflect.TypeRepr, quoted.Expr[_])] =
      import quotes.reflect._
      Lambda1.Term.unapply(expr.asTerm).map((str, tpe, expr) => (str, tpe, expr.asExpr))

    // TODO I like this pattern of doing 'Term' in a sub-object should do more of this in future
    object Term {
      def unapply(using Quotes)(term: quotes.reflect.Term): Option[(String, quotes.reflect.TypeRepr, quotes.reflect.Term)] =
        import quotes.reflect._
        Untype(term) match {
          case Lambda(List(ValDef(ident, tpeTree, _)), methodBody) => Some((ident, tpeTree.tpe, methodBody))
          case Block(List(), expr) => Lambda1.Term.unapply(expr)
          case _ => None
        }
    }
  }

  object Lambda2 {
    def unapply(using Quotes)(expr: Expr[_]): Option[(String, quotes.reflect.TypeRepr, String, quotes.reflect.TypeRepr, quoted.Expr[_])] =
      import quotes.reflect._
      unapplyTerm(expr.asTerm).map((str1, tpe1, str2, tpe2, expr) => (str1, tpe1, str2, tpe2, expr.asExpr))

    def unapplyTerm(using Quotes)(term: quotes.reflect.Term): Option[(String, quotes.reflect.TypeRepr, String, quotes.reflect.TypeRepr, quotes.reflect.Term)] =
      import quotes.reflect._
      Untype(term) match {
        case Lambda(List(ValDef(ident1, tpe1, _), ValDef(ident2, tpe2, _)), methodBody) => Some((ident1, tpe1.tpe, ident2, tpe2.tpe, methodBody))
        case Block(List(), expr) => unapplyTerm(expr)
        case _ => None
      }
  }

  object RawLambdaN {
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[(List[(String, quotes.reflect.TypeRepr)], quotes.reflect.Term)] =
      import quotes.reflect._
      Untype(term) match {
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
    def unapply(using Quotes)(term: Expr[_]): Option[(List[(String, quotes.reflect.TypeRepr)], quoted.Expr[_])] =
      import quotes.reflect._
      RawLambdaN.unapply(term.asTerm).map((strAndTpe, term) => (strAndTpe, term.asExpr))
  }

  // object Lambda2 {
  //   def unapply(using Quotes)(term: Expr[_]): Option[(String, String, quoted.Expr[_])] = term match {
  //     case Unseal(Lambda(List(ValDef(ident, _, _), ValDef(ident2, _, _)), Seal(methodBody))) => Some((ident, ident2, methodBody))
  //     case _ => None
  //   }
  // }

  object Unseal {
    def unapply(using Quotes)(t: Expr[Any]): Option[quotes.reflect.Term] =
      import quotes.reflect._
      Some(t.asTerm)
  }
  object Seal {
    def apply[T](using Quotes)(e: quotes.reflect.Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.asType.asInstanceOf[quoted.Type[T]] // FIXME: this cast is unsound
      e.asExprOf[T]
    }

    def unapply[T](using Quotes)(e: quotes.reflect.Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.asType.asInstanceOf[quoted.Type[T]] // FIXME: this cast is unsound
      Some(e.asExprOf[T])
    }
  }

  object TupleName {
    def unapply(str: String): Boolean = str.matches("Tuple[0-9]+")
  }
  object TupleIdent {
    def unapply(using Quotes)(term: quotes.reflect.Term): Boolean =
      import quotes.reflect._
      term match {
        case Ident(TupleName()) => true
        case _ => false
      }
  }

  object UntypeApply {
    private def recurse(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term = {
      import quotes.reflect._
      //println("============== Recursing UntypeApply =============")
      term match {
        case TypeApply(content, args) => recurse(content)
        case other => other
      }
    }
    def unapply(using Quotes)(term: quotes.reflect.Term) = Some(recurse(term))
  }

  // object UntypeAll {
  //   def unapply(term: Term) =
  //     Untype(UntypeApply(term))
  // }

  object Method0 {
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[(quotes.reflect.Term, String)] =
      import quotes.reflect._
      term match {
        case UntypeApply(Select(source, methodName)) => Some((source, methodName))
        case _ => None
      }
  }

  object UntypeTree {
    def recurse(using Quotes)(innerTerm: quotes.reflect.Tree): quotes.reflect.Tree =
      import quotes.reflect._
      innerTerm match {
        case Typed(innerTree, _) => recurse(innerTree)
        case other => other
      }

    def unapply(using Quotes)(term: quotes.reflect.Tree): Option[quotes.reflect.Tree] = Some(recurse(term))
    def apply(using Quotes)(term: quotes.reflect.Tree) = UntypeTree.unapply(term).get
  }

  /** Summon a named method from the context Context[D, N] */
  def summonContextMethod(using Quotes)(name: String, ctx: Expr[_]) = {
    import quotes.reflect._
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

    def apply[T <: ConstantValue.Kind](using Quotes)(const: T): Expr[T]  =
      const match
        case v: String => Expr(v)
        case v: Char => Expr(v)
        case v: Int => Expr(v)
        case v: Long => Expr(v)
        case v: Boolean => Expr(v)
        case v: Float => Expr(v)
        case v: Double => Expr(v)
        case v: Byte => Expr(v)

    def unapply[T <: ConstantValue.Kind](using Quotes)(t: Expr[T]) =
      t match
        case ConstExpr(v) => Some(v)
        case _ => None


  object ConstantTerm:
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[ConstantValue.Kind] =
      import quotes.reflect._
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
    def unapply(using Quotes)(expr: Expr[Any]): Option[quotes.reflect.Symbol] =
      import quotes.reflect._
      expr.asTerm.tpe.classSymbol

  object ClassSymbolAndUnseal:
    def unapply(using Quotes)(expr: Expr[Any]): Option[(quotes.reflect.Symbol, quotes.reflect.Term)] =
      import quotes.reflect._
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
      def unapply(using Quotes)(term: quotes.reflect.Term) =
        import quotes.reflect._
        term match
          case Apply(Select(New(TypeIdent(moduleType)), "<init>"), list) if (list.length == 0) && moduleType.endsWith("$") => true
          case Select(This(outerClass), name)  => true
          case Ident(name) => true
          case _ => false

    def unapply(using Quotes)(expr: Expr[Any]): Option[(String, List[String], List[Expr[Any]])] = {
      import quotes.reflect._
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
  def isType[T: Type](using Quotes)(expr: Expr[_]) =
    import quotes.reflect._
    expr.asTerm.tpe <:< TypeRepr.of[T]

  def isType[T: Type](using Quotes)(term: quotes.reflect.Term) =
    import quotes.reflect._
    term.tpe <:< TypeRepr.of[T]

  def isPrimitive(using Quotes)(tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect._
    tpe <:< TypeRepr.of[Int] ||
    tpe <:< TypeRepr.of[Long] ||
    tpe <:< TypeRepr.of[Float] ||
    tpe <:< TypeRepr.of[Double] ||
    tpe <:< TypeRepr.of[Byte] ||
    tpe <:< TypeRepr.of[Char]

  def isNumeric(using Quotes)(tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect._
    tpe <:< TypeRepr.of[Int] ||
    tpe <:< TypeRepr.of[Long] ||
    tpe <:< TypeRepr.of[Float] ||
    tpe <:< TypeRepr.of[Double] ||
    tpe <:< TypeRepr.of[scala.math.BigDecimal] ||
    tpe <:< TypeRepr.of[java.math.BigDecimal]

  // TODO Change to 'are'
  def is[T: Type](using Quotes)(inputs: Expr[_]*): Boolean =
    import quotes.reflect._
    inputs.forall(input => input.asTerm.tpe <:< TypeRepr.of[T])

  /** 
   * Uninline the term no matter what (TODO should reove the unapply case) that pattern always matches
   * and is too confusing
   */
  object Uninline {
    def unapply[T: Type](using Quotes)(any: Expr[T]): Option[Expr[T]] =
      import quotes.reflect.{ Term => _, _ }
      Some(Term.apply(any.asTerm).asExprOf[T])
    def apply[T: Type](using Quotes)(any: Expr[T]): Expr[T] =
      import quotes.reflect.{ Term => _, _ }
      Term.apply(any.asTerm).asExprOf[T]

    object Term:
      def unapply(using Quotes)(any: quotes.reflect.Term): Option[quotes.reflect.Term] =
        Some(Term.apply(any))
      def apply(using Quotes)(any: quotes.reflect.Term): quotes.reflect.Term =
        import quotes.reflect._
        any match
          // Just take the value if it is inlined. Since there could be multiple inline layers, keep un-inlining until the term is not an inline
          case Inlined(_, _, v) => Uninline.Term(v)
          case _ => any
  }

  object ConstExpr {
    /** Matches expressions containing literal constant values and extracts the value.
     *
     *  - Converts expression containg literal values to their values:
     *    - `'{1}` -> `1`, `'{2}` -> `2`, ...
     *    - For all primitive types and `String`
     *
     *  Usage:
     *  ```
     *  case '{ ... ${expr @ ConstExpr(value)}: T ...} =>
     *    // expr: Expr[T]
     *    // value: T
     *  ```
     *
     *  To directly unlift an expression `expr: Expr[T]` consider using `expr.unlift`/`expr.unliftOrError` insead.
     */
    def unapply[T](expr: Expr[T])(using Quotes): Option[T] = {
      import quotes.reflect._
      def rec(tree: Term): Option[T] = tree match {
        case Literal(c) =>
          c match
            // case Constant.Null() => None
            // case Constant.Unit() => None
            // case Constant.ClassOf(_) => None
            case _ => Some(c.value.asInstanceOf[T])
        case Block(Nil, e) => rec(e)
        case Typed(e, _) => rec(e)
        case Inlined(_, Nil, e) => rec(e)
        case _  => None
      }
      rec(expr.asTerm)
    }
  }
}