package io.getquill.parser

import io.getquill.ast.{Ident => AIdent, Query => AQuery, _}

import scala.quoted._
import scala.quoted._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable
import io.getquill.Dsl
import scala.reflect.ClassTag
import io.getquill.norm.capture.AvoidAliasConflict
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.EntityQuery
import io.getquill.Query
import io.getquill.util.Format
import io.getquill.parser.ParserHelpers._
import io.getquill.quat.QuatMaking
import io.getquill.quat.Quat
import io.getquill.metaprog.Extractors._

object ParserHelpers {

  trait Idents extends QuatMaking {
    import qctx.reflect.{Ident => TIdent, ValDef => TValDef, _}

    def cleanIdent(name: String, quat: Quat): AIdent = AIdent(name.replace("_$", "x"), quat)
    def cleanIdent(name: String, tpe: TypeRepr): AIdent = AIdent(name.replace("_$", "x"), InferQuat.ofType(tpe))
  }

  trait Assignments extends Idents {
    import qctx.reflect.{Ident => TIdent, ValDef => TValDef, _}
    import Parser.Implicits._
    import io.getquill.util.Interpolator
    import io.getquill.util.Messages.TraceType
    import io.getquill.norm.BetaReduction

    def astParse: SealedParser[Ast]

    object AssignmentTerm {
      object Components:
        def unapply(expr: Expr[_]) =
          UntypeExpr(expr) match
            case Lambda1(ident, identTpe, '{ type v; ($prop: Any).->[`v`](($value: `v`)) }) => Some((ident, identTpe, prop, value))
            case _ => None

      def verifyTypesSame(expr: Expr[_]) =
        expr match
          case Components(_, _, prop, value) =>
            val valueTpe = value.asTerm.tpe.widen
            val propTpe = prop.asTerm.tpe.widen
            if (!(valueTpe <:< propTpe))
              report.throwError(s"The ${Format.TypeRepr(valueTpe)} value ${Format.Expr(value)} cannot be assigned to the ${Format.TypeRepr(propTpe)} property ${Format.Expr(prop)} because they are not the same type (or a subtype).", expr)
          case other =>
            report.throwError(s"The assignment statement ${Format.Expr(expr)} is invalid.")

      def OrFail(expr: Expr[_]) =
        unapply(expr).getOrElse { Parser.throwExpressionError(expr, classOf[Assignment]) }

      def unapply(expr: Expr[_]): Option[Assignment] =
        UntypeExpr(expr) match
          case Components(ident, identTpe, prop, value) => 
            Some(Assignment(cleanIdent(ident, identTpe), astParse(prop), astParse(value)))
          case _ => None
    }
  }


  trait PropertyAliases(implicit val qctx: Quotes) {
    import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
    import Parser.Implicits._
    import io.getquill.util.Interpolator
    import io.getquill.util.Messages.TraceType
    import io.getquill.norm.BetaReduction

    def astParse: SealedParser[Ast]

    object PropertyAliasExpr {
      def OrFail[T: Type](expr: Expr[Any]) = expr match
          case PropertyAliasExpr(propAlias) => propAlias
          case _ => Parser.throwExpressionError(expr, classOf[PropertyAlias])

      def unapply[T: Type](expr: Expr[Any]): Option[PropertyAlias] = expr match
        case Lambda1(_, _, '{ ($prop: Any).->[v](${ConstExpr(alias: String)}) } ) =>
          def path(tree: Expr[_]): List[String] =
            tree match
              case a`.`b =>
                path(a) :+ b
              case '{ (${a`.`b}: Option[t]).map[r](${Lambda1(arg, tpe, body)}) } =>
                path(a) ++ (b :: path(body))
              case _ =>
                Nil
          end path
          Some(PropertyAlias(path(prop), alias))
        case _ =>
          None
    }
  }


  /**
   * Helpers for different behaviors Quill supports of object equality. This is non-trivial since Quill has to make sense
   * of different equality paradigms across ANSI-SQL and Scala for objects that may be Optional or not. Several
   * techniques are implemented to resolve these inconsistencies.
   */
  trait ComparisonTechniques(implicit val qctx: Quotes) {
    import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
    import Parser.Implicits._

    // To be able to access the external parser the extends this
    def astParse: SealedParser[Ast]

    sealed trait EqualityBehavior { def operator: BinaryOperator }
    case object Equal extends EqualityBehavior { def operator: BinaryOperator = EqualityOperator.`==` }
    case object NotEqual extends EqualityBehavior { def operator: BinaryOperator = EqualityOperator.`!=` }

    /**
     * Taken from the identically named method in Parser.scala in Scala2-Quill. Much of this logic
     * is not macro specific so a good deal of it can be refactored out into the quill-sql-portable module.
     * Do equality checking on the database level with the same truth-table as idiomatic scala
     */
    def equalityWithInnerTypechecksIdiomatic(left: Term, right: Term)(equalityBehavior: EqualityBehavior) = {
      import io.getquill.ast.Implicits._
      val (leftIsOptional, rightIsOptional) = checkInnerTypes(left, right, ForbidInnerCompare)
      val a = astParse(left.asExpr)
      val b = astParse(right.asExpr)
      val comparison = BinaryOperation(a, equalityBehavior.operator, b)
      (leftIsOptional, rightIsOptional, equalityBehavior) match {
        // == two optional things. Either they are both null or they are both defined and the same
        case (true, true, Equal)    => (OptionIsEmpty(a) +&&+ OptionIsEmpty(b)) +||+ (OptionIsDefined(a) +&&+ OptionIsDefined(b) +&&+ comparison)
        // != two optional things. Either one is null and the other isn't. Or they are both defined and have different values
        case (true, true, NotEqual) => (OptionIsDefined(a) +&&+ OptionIsEmpty(b)) +||+ (OptionIsEmpty(a) +&&+ OptionIsDefined(b)) +||+ comparison
        // No additional logic when both sides are defined
        case (false, false, _)      => comparison
        // Comparing an optional object with a non-optional object is not allowed when using scala-idiomatic optional behavior
        case (lop, rop, _) => {
          val lopString = (if (lop) "Optional" else "Non-Optional") + s" ${left}}"
          val ropString = (if (rop) "Optional" else "Non-Optional") + s" ${right}}"
          report.throwError(s"Cannot compare ${lopString} with ${ropString} using operator ${equalityBehavior.operator}", left.asExpr)
        }
      }
    }

    /**
     * (not used yet but will be used when support for 'extras' dsl functionality is added)
     * Do equality checking on the database level with the ansi-style truth table (i.e. always false if one side is null)
     */
    def equalityWithInnerTypechecksAnsi(left: Term, right: Term)(equalityBehavior: EqualityBehavior) = {
      import io.getquill.ast.Implicits._
      val (leftIsOptional, rightIsOptional) = checkInnerTypes(left, right, AllowInnerCompare)
      val a = astParse(left.asExpr)
      val b = astParse(right.asExpr)
      val comparison = BinaryOperation(a, equalityBehavior.operator, b)
      (leftIsOptional, rightIsOptional) match {
        case (true, true)   => OptionIsDefined(a) +&&+ OptionIsDefined(b) +&&+ comparison
        case (true, false)  => OptionIsDefined(a) +&&+ comparison
        case (false, true)  => OptionIsDefined(b) +&&+ comparison
        case (false, false) => comparison
      }
    }

    trait OptionCheckBehavior
    /** Allow T == Option[T] comparison **/
    case object AllowInnerCompare extends OptionCheckBehavior
    /** Forbid T == Option[T] comparison **/
    case object ForbidInnerCompare extends OptionCheckBehavior

    /**
     * Type-check two trees, if one of them has optionals, go into the optionals to find the root types
     * in each of them. Then compare the types that are inside. If they are not compareable, abort the build.
     * Otherwise return type of which side (or both) has the optional. In order to do the actual comparison,
     * the 'weak conformance' operator is used and a subclass is allowed on either side of the `==`. Weak
     * conformance is necessary so that Longs can be compared to Ints etc...
     */
    def checkInnerTypes(lhs: Term, rhs: Term, optionCheckBehavior: OptionCheckBehavior): (Boolean, Boolean) = {
      val leftType = lhs.tpe
      val rightType = rhs.tpe
      // Note that this only goes inside the optional one level i.e. Option[T] => T. If we have Option[Option[T]] it will return the inside Option[T].
      // This is by design. If the types do not match, even if we normally don't care about the outer layer
      // (i.e. for equalityWithInnerTypechecksAnsi where Option[T] == T is allowed (that's in the 'extras' modules which uses ===))
      // we still want to fail with an exception that the types are identical if the user does Option[Option[T]] == Option[T] since that is a serious
      // typing error.
      val leftInner = innerOptionParam(leftType)
      val rightInner = innerOptionParam(rightType)
      val leftIsOptional = isOptionType(leftType) && !(leftType.widen =:= TypeRepr.of[Nothing]) && !(leftType.widen =:= TypeRepr.of[Null])
      val rightIsOptional = isOptionType(rightType) && !(rightType.widen =:= TypeRepr.of[Nothing]) && !(rightType.widen =:= TypeRepr.of[Null])
      val typesMatch = wideMatchTypes(rightInner, leftInner)

      optionCheckBehavior match {
        case AllowInnerCompare if typesMatch =>
          (leftIsOptional, rightIsOptional)
        case ForbidInnerCompare if ((leftIsOptional && rightIsOptional) || (!leftIsOptional && !rightIsOptional)) && typesMatch =>
          (leftIsOptional, rightIsOptional)
        case _ =>
          if (leftIsOptional || rightIsOptional)
            report.throwError(s"${Format.TypeReprW(leftType)} == ${Format.TypeReprW(rightType)} is not allowed since ${Format.TypeReprW(leftInner)}, ${Format.TypeReprW(rightInner)} are different types.", lhs.asExpr)
          else
            report.throwError(s"${Format.TypeReprW(leftType)} == ${Format.TypeReprW(rightType)} is not allowed since they are different types.", lhs.asExpr)
      }
    }

    def isOptionType(tpe: TypeRepr) = tpe <:< TypeRepr.of[Option[_]]

    /**
     * Match types in the most wide way possible. This function is not for generalized type equality since quill does not directly
     * compare anything, rather it just translates things into SQL expressions. This kind of check is used in a general sense when things
     * that it doesn't even make sense to compare are compared e.g. an Person and a String. In this case, we want to provide some kind
     * of compile-time warning that the comparision the user is attempting to do in SQL is non sensical in the first place. Therefore when
     * there is any kind of possibility that the expression makes sense (e.g. by comparing a Dog to a Animal (i.e. class to subclass), by comparing
     * two numeric types of any kind etc... we allow the comparison to happen).
     * For int/long/float/double comparisons don't crash on compile-time typing can re-evaluate this upon user feedback
     */
    def wideMatchTypes(a: TypeRepr, b: TypeRepr) =
      a.widen =:= b.widen || a.widen <:< b.widen || b.widen <:< a.widen || (isNumeric(a.widen) && isNumeric(b.widen))

    def innerOptionParam(tpe: TypeRepr): TypeRepr =
      if (tpe <:< TypeRepr.of[Option[_]])
        tpe.asType match
          case '[Option[t]] => TypeRepr.of[t]
      else
        tpe
  }

  trait PatternMatchingValues(implicit override val qctx: Quotes) extends QuatMaking {
    import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
    import Parser.Implicits._
    import io.getquill.util.Interpolator
    import io.getquill.util.Messages.TraceType
    import io.getquill.norm.BetaReduction

    def astParse: SealedParser[Ast]

    // don't change to ValDef or might override the real valdef in qctx.reflect
    object ValDefTerm {
      def unapply(tree: Tree): Option[Ast] =
        tree match {
          case TValDef(name, tpe, Some(t @ PatMatchTerm(ast))) =>
            println(s"====== Parsing Val Def ${name} = ${t.show}")
            Some(Val(AIdent(name, InferQuat.ofType(tpe.tpe)), ast))

          // In case a user does a 'def' instead of a 'val' with no paras and no types then treat it as a val def
          // this is useful for things like (TODO Get name) where you'll have something like:
          // query[Person].map(p => (p.name, p.age)).filter(tup => tup._1.name == "Joe")
          // But in Scala3 you can do:
          // query[Person].map(p => (p.name, p.age)).filter((name, age) => name == "Joe")
          // Then in the AST it will look something like:
          // query[Person].map(p => (p.name, p.age)).filter(x$1 => { val name=x$1._1; val age=x$1._2; name == "Joe" })
          // and you need to resolve the val defs thare are created automatically
          case DefDef(name, paramss, tpe, rhsOpt) if (paramss.length == 0) =>
            //println(s"====== Parsing Def Def ${name} = ${rhsOpt.map(_.show)}")
            val body =
              rhsOpt match {
                // TODO Better site-description in error
                case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
                case Some(rhs) => rhs
              }
            val bodyAst = astParse(body.asExpr)
            Some(Val(AIdent(name, InferQuat.ofType(tpe.tpe)), bodyAst))

          case TValDef(name, tpe, rhsOpt) =>
            val body =
              rhsOpt match {
                // TODO Better site-description in error
                case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
                case Some(rhs) => rhs
              }
            val bodyAst = astParse(body.asExpr)
            Some(Val(AIdent(name, InferQuat.ofType(tpe.tpe)), bodyAst))

          case _ => None
        }
    }

    object PatMatchTerm {
      def unapply(term: Term): Option[Ast] =
        term match {
          case Match(expr, List(CaseDef(fields, guard, body))) =>
            guard match {
              case Some(guardTerm) => report.throwError("Guards in case- match are not supported", guardTerm.asExpr)
              case None =>
            }
            Some(patMatchParser(expr, fields, body))

          case other => None
        }
    }

    protected def patMatchParser(tupleTree: Term, fieldsTree: Tree, bodyTree: Term): Ast = {
      val tuple = astParse(tupleTree.asExpr)
      val body = astParse(bodyTree.asExpr)

      /*
      Get a list of all the paths of all the identifiers inside the tuple. E.g:
      foo match { case ((a,b),c) => bar } would yield something like:
      List((a,List(_1, _1)), (b,List(_1, _2)), (c,List(_2)))
      */
      def tupleBindsPath(field: Tree, path: List[String] = List()): List[(AIdent, List[String])] = {
        UntypeTree(field) match {
          case Bind(name, TIdent(_)) => List(AIdent(name) -> path)
          case Unapply(Method0(TupleIdent(), "unapply"), something, binds) =>
            binds.zipWithIndex.flatMap { case (bind, idx) =>
              tupleBindsPath(bind, path :+ s"_${idx + 1}")
            }
          case other => report.throwError(s"Invalid Pattern Matching Term: ${Printer.TreeStructure.show(other)}")
        }
      }

      /* Take the list found in the tupleBindsPath method above and match up each match-tuple element
      from the original tuple we found. For example, if we had: foo match { case ((a,b),c) => bar }
      we get something like List((a,List(_1, _1)), (b,List(_1, _2)), (c,List(_2))). If 'foo'
      is ((f,b),z) then we want to get: List(((f,b),z)._1._1, ((f,b),z)._1._2, ((f,b),z)._2)
      */
      def propertyAt(path: List[String]) =
        path.foldLeft(tuple) {
          case (tup, elem) => Property(tup, elem)
        }

      val fieldPaths = tupleBindsPath(fieldsTree)
      val reductionTuples = fieldPaths.map((id, path) => (id, propertyAt(path)))

      val interp = new Interpolator(TraceType.Standard, 1)
      import interp._

      trace"Pat Match Parsing: ${body}".andLog()
      trace"Reductions: ${reductionTuples}".andLog()
      // Do not care about types here because pat-match body does not necessarily have correct typing in the Parsing phase
      BetaReduction(body, reductionTuples: _*)
    }
  }

}