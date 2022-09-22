package io.getquill.metaprog

import scala.quoted._
import scala.quoted.Varargs
import io.getquill.util.Format
import io.getquill.util.Messages.TraceType

class Is[T: Type]:
  def unapply(expr: Expr[Any])(using Quotes) =
    import quotes.reflect._
    if (expr.asTerm.tpe <:< TypeRepr.of[T])
      Some(expr.asExprOf[T])
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

  object SelectApplyN {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[_], String, List[Expr[_]])] =
      import quotes.reflect._
      SelectApplyN.Term.unapply(term.asTerm).map((sub, method, obj) => (sub.asExpr, method, obj.map(_.asExpr)))

    object Term:
      def unapply(using Quotes)(term: quotes.reflect.Term): Option[(quotes.reflect.Term, String, List[quotes.reflect.Term])] =
        import quotes.reflect._
        term match
          // case Apply(Select(body, method), args) => Some((body, method, args))
          // case Apply(TypeApply(Select(body, method), _), args) => Some((body, method, args))
          case Applies(Select(body, method), args) => Some((body, method, args))
          case _                                  => None
  }

  /**
   * Matches predicate(bar) or predicate[T](bar)
   * where predicate can be a simple method or something selected from something else e.g:
   * foo.method(bar) or foo.method[T](bar)
   */
  object Applies:
    def unapply(using Quotes)(term: quotes.reflect.Term) =
      import quotes.reflect._
      term match
        // TypeApply predicate has to be first because the 2nd one with match everything
        case Apply(TypeApply(predicate, _), args) => Some((predicate, args))
        case Apply(predicate, args)               => Some((predicate, args))
        case _                                    => None

  object SelectApply1 {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[_], String, Expr[_])] =
      import quotes.reflect._
      term match {
        case Unseal(Applies(Select(body, method), List(arg))) => Some((body.asExpr, method, arg.asExpr))
        case _                                               => None
      }
  }

  // Designed to be a more generic version the Varargs which does not handle all cases.
  // Particularly when a varargs parameter is passed from one inline function into another.
  object GenericSeq {
    def unapply(using Quotes)(term: Expr[_]): Option[List[Expr[_]]] = {
      import quotes.reflect._
      term match {
        case Varargs(props)                     => Some(props.toList)
        case '{ List(${ Varargs(props) }) }     => Some(props.toList)
        case '{ Nil }                           => Some(List())
        case '{ Seq(${ Varargs(props) }) }      => Some(props.toList)
        case Unseal(Untype(Repeated(props, _))) => Some(props.map(_.asExpr))
        case other =>
          report.throwError("Could not parse sequence expression:\n" + Format.Term(term.asTerm))
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
      import scala.util.{Try, Success, Failure}
      Untype.unapply(expr.asTerm).map(_.asExpr).get
  }

  // Always match (whether ast starts with Typed or not). If it does, strip the Typed node.
  object Untype {
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] = term match {
      case TypedMatryoshkaTerm(t) => Some(t)
      case other                 => Some(other)
    }

    def apply(using Quotes)(term: quotes.reflect.Term) = Untype.unapply(term).get
  }

  /**
   * Ignore case where there happens to be an apply e.g. java functions where "str".length in scala
   * will translate into "str".lenth() since for java methods () is automatically added in.
   * Hence it's `Apply( Select(Literal(IntConstant("str")), "length") )`
   * Not just `Select(Literal(IntConstant("str")), "length")`
   *
   * Note maybe there's even a case where you want multiple empty-applies e.g. foo()() to be ignored
   * hence this would be done recursively like `Untype`
   */
  object IgnoreApplyNoargs {
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect._
      term match {
        case Apply(inner, Nil) => Some(inner)
        case _                 => Some(term)
      }
  }

  object TypedMatryoshkaTerm {
    def recurse(using Quotes)(innerTerm: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect._
      innerTerm match
        case Typed(innerTree, _) => recurse(innerTree)
        case other               => other

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect._
      term match
        case Typed(tree, _) => Some(recurse(tree))
        case other          => None
  }

  object TypedMatryoshka {
    def unapply(using Quotes)(term: Expr[Any]): Option[Expr[Any]] =
      import quotes.reflect._
      TypedMatryoshkaTerm.unapply(term.asTerm).map(_.asExpr)
  }

  object SelectExpr {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[_], String)] =
      import quotes.reflect._
      term match {
        case Unseal(Select(Seal(prefix), memberName)) => Some((prefix, memberName))
        case _                                        => None
      }
  }

  object `.` {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[_], String)] =
      import quotes.reflect._
      term match {
        case Unseal(Select(Seal(prefix), memberName)) => Some((prefix, memberName))
        case _                                        => None
      }
  }

  extension (expr: Expr[_]) {
    def `.(caseField)`(property: String)(using Quotes) = {
      import quotes.reflect._
      val tpe = expr.asTerm.tpe
      val cls = tpe.widen.typeSymbol
      if (!cls.flags.is(Flags.Case))
        report.throwError(
          s"The class ${Format.TypeRepr(expr.asTerm.tpe)} (symbol: ${cls}) is not a case class in the expression: ${Format.Expr(expr)}\n" +
            s"Therefore you cannot lookup the property `${property}` on it!"
        )
      else
        val method =
          cls.caseFields
            .find(sym => sym.name == property)
            .getOrElse {
              report.throwError(s"Cannot find property '${property}' of (${expr.show}:${cls.name}) fields are: ${cls.caseFields.map(_.name)}", expr)
            }

        '{ (${ Select(expr.asTerm, method).asExpr }) }
    }
  }

  object SelectExprOpt {
    def unapply(using Quotes)(term: Expr[_]): Option[(Expr[Option[_]], String)] =
      import quotes.reflect._
      term match {
        case Unseal(Select(prefix, memberName)) => Some((prefix.asExprOf[Option[Any]], memberName))
        case _                                  => None
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
          case Block(List(), expr)                                 => Lambda1.Term.unapply(expr)
          case _                                                   => None
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
        case Block(List(), expr)                                                        => unapplyTerm(expr)
        case _                                                                          => None
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
        case _                   => None
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

  object ArrowFunction:
    def unapply(expr: Expr[_])(using Quotes) =
      import quotes.reflect._
      expr match
        case '{ type v; ($prop: Any).->[`v`](($value: `v`)) } => Some((prop, value))
        case _                                                => None

  object TupleName {
    def unapply(str: String): Boolean = str.matches("Tuple[0-9]+")
  }
  object TupleIdent {
    def unapply(using Quotes)(term: quotes.reflect.Term): Boolean =
      import quotes.reflect._
      term match {
        case Ident(TupleName()) => true
        case _                  => false
      }
  }

  object UntypeApply {
    private def recurse(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term = {
      import quotes.reflect._
      // println("============== Recursing UntypeApply =============")
      term match {
        case TypeApply(content, args) => recurse(content)
        case other                    => other
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
        case _                                       => None
      }
  }

  object UntypeTree {
    def recurse(using Quotes)(innerTerm: quotes.reflect.Tree): quotes.reflect.Tree =
      import quotes.reflect._
      innerTerm match {
        case Typed(innerTree, _) => recurse(innerTree)
        case other               => other
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
        case _: Char    => Some(any.asInstanceOf[Kind])
        case _: Int     => Some(any.asInstanceOf[Kind])
        case _: Long    => Some(any.asInstanceOf[Kind])
        case _: Boolean => Some(any.asInstanceOf[Kind])
        case _: Float   => Some(any.asInstanceOf[Kind])
        case _: Double  => Some(any.asInstanceOf[Kind])
        case _: Byte    => Some(any.asInstanceOf[Kind])
        case _          => None
      }

  object ConstantExpr:
    // def Any(v: Any): Expr[Any] =
    //   v match
    //     case cv: String | Char | Int | Long | Boolean | Float | Double | Byte => apply(cv)
    // case _ => report.throwError(s"Cannot lift constant value: ${v}, it is not one of the allowed constant types: String | Int | Long | Boolean | Float | Double | Byte")

    def apply[T <: ConstantValue.Kind](using Quotes)(const: T): Expr[T] =
      const match
        case v: String  => Expr(v)
        case v: Char    => Expr(v)
        case v: Int     => Expr(v)
        case v: Long    => Expr(v)
        case v: Boolean => Expr(v)
        case v: Float   => Expr(v)
        case v: Double  => Expr(v)
        case v: Byte    => Expr(v)

    def unapply[T <: ConstantValue.Kind](using Quotes)(t: Expr[T]) =
      t match
        case ConstExpr(v) => Some(v)
        case _            => None

  object ConstantTerm:
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[ConstantValue.Kind] =
      import quotes.reflect._
      term match
        case Literal(StringConstant(v: String))   => Some(v)
        case Literal(IntConstant(v: Int))         => Some(v)
        case Literal(LongConstant(v: Long))       => Some(v)
        case Literal(BooleanConstant(v: Boolean)) => Some(v)
        case Literal(FloatConstant(v: Float))     => Some(v)
        case Literal(DoubleConstant(v: Double))   => Some(v)
        case Literal(ByteConstant(v: Byte))       => Some(v)
        case _                                    => None

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
          case Select(This(outerClass), name)                                                                              => true
          case Ident(name)                                                                                                 => true
          case _                                                                                                           => false

    def unapply(using Quotes)(expr: Expr[Any]): Option[(String, List[String], List[Expr[Any]])] = {
      import quotes.reflect._
      // lazy val tpe = expr.asTerm.tpe
      // lazy val companionClass = tpe.classSymbol.get.companionClass
      // lazy val name = tpe.classSymbol.get.name
      // lazy val fields = tpe.classSymbol.get.caseFields.map(_.name) // Don't actually evaluate them unless it matches
      // println(s"@@@@@@@@@@@@@@ ***************** TRYING CASE CLASS CREATE ***************** @@@@@@@@@@@@@@\n" + Printer.TreeStructure.show(expr.asTerm))

      // def companionIsProduct(classSymbol: Symbol) = expr.asTerm.tpe.select(classSymbol.companionClass) <:< TypeRepr.of[Product]
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

          // When constructing a case class with a macro we sometimes get this (e.g using ConstructType)
          case ClassSymbolAndUnseal(sym, Apply(Select(New(Inferred()), "<init>"), args)) if isType[Product](expr) =>
            Some((sym.name, sym.caseFields.map(_.name), args.map(_.asExpr)))
          case ClassSymbolAndUnseal(sym, Apply(Select(New(TypeIdent(_)), "<init>"), args)) if isType[Product](expr) =>
            Some((sym.name, sym.caseFields.map(_.name), args.map(_.asExpr)))
          case ClassSymbolAndUnseal(sym, Apply(Select(ModuleCreation(), "apply"), args)) if isType[Product](expr) => // && sym.flags.is(Flags.Case)
            Some((sym.name, sym.caseFields.map(_.name), args.map(_.asExpr)))
          case _ =>
            None

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
    tpe <:< TypeRepr.of[String] ||
    tpe <:< TypeRepr.of[Int] ||
    tpe <:< TypeRepr.of[Short] ||
    tpe <:< TypeRepr.of[Long] ||
    tpe <:< TypeRepr.of[Float] ||
    tpe <:< TypeRepr.of[Double] ||
    tpe <:< TypeRepr.of[Byte] ||
    tpe <:< TypeRepr.of[Char]

  def isNumeric(using Quotes)(tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect._
    tpe <:< TypeRepr.of[Int] ||
    tpe <:< TypeRepr.of[Long] ||
    tpe <:< TypeRepr.of[Short] ||
    tpe <:< TypeRepr.of[Float] ||
    tpe <:< TypeRepr.of[Double] ||
    tpe <:< TypeRepr.of[scala.math.BigDecimal] ||
    tpe <:< TypeRepr.of[java.math.BigDecimal]

  def isNumericPrimitive(using Quotes)(tpe: quotes.reflect.TypeRepr) =
    isNumeric(tpe) && isPrimitive(tpe)

  /**
   * Check whether one numeric `from` can be primitively assigned to a variable of another `into`
   * i.e. short can fit into a int, int can fit into a long. Same with float into a double.
   * This is used to determine what can be assigned into what (e.g. in a insert(_.age -> 4.toShort) statement)
   * and still be considered a valid transpilation.
   */
  def numericPrimitiveFitsInto(using Quotes)(into: quotes.reflect.TypeRepr, from: quotes.reflect.TypeRepr) =
    import quotes.reflect._
    def score(tpe: TypeRepr) =
      if (tpe <:< TypeRepr.of[Short]) 1
      else if (tpe <:< TypeRepr.of[Int]) 3
      else if (tpe <:< TypeRepr.of[Long]) 7 // short fits into float fits into long
      else if (tpe <:< TypeRepr.of[Float]) 16
      else if (tpe <:< TypeRepr.of[Double]) 24 // float fits into double
      else 0
    val fromScore = score(from)
    val intoScore = score(into)
    (intoScore & fromScore) != 0 && intoScore >= fromScore

  // TODO Change to 'are'
  def is[T: Type](using Quotes)(inputs: Expr[_]*): Boolean =
    import quotes.reflect._
    inputs.forall(input => input.asTerm.tpe <:< TypeRepr.of[T])

  object `Option[...[t]...]`:
    def innerOrTopLevelT(tpe: Type[_])(using Quotes): Type[_] =
      tpe match
        case '[Option[t]] => innerOrTopLevelT(Type.of[t])
        case '[t]         => Type.of[t]
    def innerT(tpe: Type[_])(using Quotes) =
      import quotes.reflect._
      tpe match
        case '[Option[t]] => innerOrTopLevelT(Type.of[t])
        case '[t]         => report.throwError(s"The Type ${Format.TypeOf[t]} is not an Option")

  object SealedInline:
    def unapply[T: Type](using Quotes)(expr: Expr[T]) =
      import quotes.reflect._
      expr.asTerm match
        case Inlined(parent, defs, v) => Some((parent, defs, v.asExprOf[T]))
        case _                        => None

  /**
   * Uninline the term no matter what (TODO should remove the unapply case) that pattern always matches
   * and is too confusing
   */
  object Uninline {
    def unapply[T: Type](using Quotes)(any: Expr[T]): Option[Expr[T]] =
      import quotes.reflect.{Term => _, _}
      Some(Term.apply(any.asTerm).asExprOf[T])
    def apply[T: Type](using Quotes)(any: Expr[T]): Expr[T] =
      import quotes.reflect.{Term => _, _}
      Term.apply(any.asTerm).asExprOf[T]

    object Term:
      def unapply(using Quotes)(any: quotes.reflect.Term): Option[quotes.reflect.Term] =
        Some(Term.apply(any))
      def apply(using Quotes)(any: quotes.reflect.Term): quotes.reflect.Term =
        import quotes.reflect._
        any match
          //
          case i @ Inlined(_, pv, v) =>
            if (SummonTranspileConfig.summonTraceTypes(true).contains(TraceType.Meta))
              report.warning(s"Ran into an inline on a clause: ${Format(Printer.TreeStructure.show(i.underlyingArgument))}. Proxy variables will be discarded: ${pv}")
            v.underlyingArgument
          case _ => any
  }

  object ConstExpr {

    /**
     * Matches expressions containing literal constant values and extracts the value.
     *
     *  - Converts expression containing literal values to their values:
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
     *  To directly unlift an expression `expr: Expr[T]` consider using `expr.unlift`/`expr.unliftOrError` instead.
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
        case Block(Nil, e)      => rec(e)
        case Typed(e, _)        => rec(e)
        case Inlined(_, Nil, e) => rec(e)
        case _                  => None
      }
      rec(expr.asTerm)
    }
  }

  def nestInline(using Quotes)(call: Option[quotes.reflect.Tree], defs: List[quotes.reflect.Definition])(expr: Expr[_]): Expr[_] =
    import quotes.reflect._
    Inlined(call, defs, expr.asTerm).asExpr

  /**
   * Since things like the QueryParser slow are because Quoted matching is slow (or at least slower then I'd like them to be),
   * a simple performance optimization is to check if there's a single-method being matched and if so, what is it's name.
   * Since Scala matches unapply causes left-to-right (nested and recursively),  we can add a unapply clause
   * that will grab the name of the method (if it is a single one being matched which in most cases of the
   * QueryParser is exactly what we're looking for) and then match it to a name that we expect it to have.
   * For example, if we're trying to match this:
   * {{
   *   case '{ ($o: Option[t]).map(${Lambda1(id, idType, body)}) } =>
   * }}
   * We can do the following:
   * {{
   *   case "map" -@> '{ ($o: Option[t]).map(${Lambda1(id, idType, body)}) } =>
   * }}
   * This will check that there's a `Apply(TypeApply(Select(_, "map"), _), _)` being called
   * and then only proceed into the quoted-matcher if that is the case.
   */
  object MatchingOptimizers:
    object --> :
      def unapply(using Quotes)(expr: Expr[_]) =
        import quotes.reflect._
        // Doing UntypeExpr will make this match foo.bar as well as foo.bar[T] but it might be slower
        expr.asTerm match
          case Select(_, methodName) =>
            Some((methodName, expr))
          case _ => None

    object -@> :
      def unapply(using Quotes)(expr: Expr[_]) =
        import quotes.reflect._
        expr.asTerm match
          case SelectApplyN.Term(_, methodName, _) =>
            Some((methodName, expr))
          case _ => None

    object -@@> :
      def unapply(using Quotes)(expr: Expr[_]) =
        import quotes.reflect._
        expr.asTerm match
          case Applies(SelectApplyN.Term(_, methodName, _), _) =>
            Some((methodName, expr))
          case _ => None
  end MatchingOptimizers
}
