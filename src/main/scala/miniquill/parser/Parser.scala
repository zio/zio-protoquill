package miniquill.parser

import io.getquill.ast.{Ident => Idnt, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.quoted.{Const => ConstExpr}
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable
import miniquill.quoter.Dsl
import scala.reflect.ClassTag
import io.getquill.norm.capture.AvoidAliasConflict
import miniquill.quoter.QuotationLotExpr
import io.getquill.EntityQuery
import io.getquill.Query
import io.getquill.Format

type Parser[R] = PartialFunction[quoted.Expr[_], R]
type SealedParser[R] = (quoted.Expr[_] => R)

object Parser {
  val empty: Parser[Ast] = PartialFunction.empty[Expr[_], Ast]

  def throwExpressionError(expr: Expr[_], astClass: Class[_])(implicit qctx: QuoteContext) =
    report.throwError(s"""|
      |s"==== Tree cannot be parsed to '${astClass.getSimpleName}' ===
      |  ${Format(expr.show)}
      |==== Extractors ===
      |  ${Format(expr.unseal.showExtractors)}
      |==== Tree ===
      |  ${printer.str(expr.unseal)}
      |""".stripMargin, 
      expr)
  

  object Implicits {

    implicit class ParserExtensions[R](parser: Parser[R])(implicit val qctx: QuoteContext, ct: ClassTag[R]) {
      import qctx.tasty._

      def seal: SealedParser[R] = 
        (expr: Expr[_]) => parser.lift(expr).getOrElse {
          throwExpressionError(expr, ct.runtimeClass)
        }        
    }
  }

  trait Delegated[R] extends Parser[R] with TastyMatchers {
    implicit val qctx:QuoteContext
    def delegate: PartialFunction[Expr[_], R]
    override def apply(expr: Expr[_]): R = {
      delegate.apply(expr)
    }
    def isDefinedAt(expr: Expr[_]): Boolean = {
      delegate.isDefinedAt(expr)
    }
  }

  trait Clause[R](implicit val qctx: QuoteContext) extends Delegated[R] with TastyMatchers { base =>
    import Implicits._

    def root: Parser[Ast]
    def astParse = root.seal
    def reparent(root: Parser[Ast]): Clause[R]
  }

  case class Series private (children: List[Clause[Ast]] = List())(override implicit val qctx: QuoteContext) extends Delegated[Ast] { self =>
    
    def delegate = composite
    
    def composite: PartialFunction[Expr[_], Ast] =
      children.map(child => child.reparent(this)).foldRight(PartialFunction.empty[Expr[_], Ast])(_ orElse _)
    
    def combine(other: Series): Series =
      Series(self.children ++ other.children)
  }

  object Series {
    def single(clause: Clause[Ast])(implicit qctx: QuoteContext): Series = Series(List(clause))
  }
}

trait ParserFactory {
  def apply(using qctx: QuoteContext): Parser[Ast]
}
trait ParserLibrary extends ParserFactory {
  import Parser._

  def quotationParser(using qctx: QuoteContext)  =         Series.single(new QuotationParser)
  def queryParser(using qctx: QuoteContext)      =         Series.single(new QueryParser)
  def patMatchParser(using qctx: QuoteContext)      =      Series.single(new CasePatMatchParser)
  def functionParser(using qctx: QuoteContext)   =         Series.single(new FunctionParser)
  def functionApplyParser(using qctx: QuoteContext) =      Series.single(new FunctionApplyParser)
  def valParser(using qctx: QuoteContext)      =           Series.single(new ValParser)
  def blockParser(using qctx: QuoteContext)      =         Series.single(new BlockParser)
  def operationsParser(using qctx: QuoteContext) =         Series.single(new OperationsParser)
  def genericExpressionsParser(using qctx: QuoteContext) = Series.single(new GenericExpressionsParser)
  def actionParser(using qctx: QuoteContext)             = Series.single(new ActionParser)
  def optionParser(using qctx: QuoteContext)             = Series.single(new OptionParser)

  // def userDefined(using qctxInput: QuoteContext) = Series(new Glosser[Ast] {
  //   val qctx = qctxInput
  //   def apply(root: Parser[Ast]) = PartialFunction.empty[Expr[_], Ast]
  // })

  def apply(using qctx: QuoteContext): Parser[Ast] = {
    quotationParser
        .combine(queryParser)
        .combine(optionParser)
        .combine(actionParser)
        .combine(functionParser) // decided to have it be it's own parser unlike Quill3
        .combine(patMatchParser)
        .combine(valParser)
        .combine(blockParser)
        .combine(operationsParser)
        .combine(functionApplyParser) // must go before genericExpressionsParser otherwise that will consume the 'apply' clauses
        .combine(genericExpressionsParser)
        }
}

object ParserLibrary extends ParserLibrary

case class FunctionApplyParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Type => TType, _}
  import Parser.Implicits._
  import io.getquill.norm.capture.AvoidAliasConflict
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  //case q"new { def apply[..$t1](...$params) = $body }" =>
  //  c.fail("Anonymous classes aren't supported for function declaration anymore. Use a method with a type parameter instead. " +
  //    "For instance, replace `val q = quote { new { def apply[T](q: Query[T]) = ... } }` by `def q[T] = quote { (q: Query[T] => ... }`")

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(Apply(Select(term, "apply"), args)) =>
      FunctionApply(astParse(term.seal), args.map(arg => astParse(arg.seal)))
  }
}


case class FunctionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Type => TType, _}
  import Parser.Implicits._
  import io.getquill.norm.capture.AvoidAliasConflict
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  //case q"new { def apply[..$t1](...$params) = $body }" =>
  //  c.fail("Anonymous classes aren't supported for function declaration anymore. Use a method with a type parameter instead. " +
  //    "For instance, replace `val q = quote { new { def apply[T](q: Query[T]) = ... } }` by `def q[T] = quote { (q: Query[T] => ... }`")

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(RawLambdaN(params, body)) =>
      val subtree = Function(params.map(Idnt(_)), astParse(body.seal))
      // If there are actions inside the subtree, we need to do some additional sanitizations
      // of the variables so that their content will not collide with code that we have generated.

      // TODO Add back once moved to the quill subtree because AvoidAliasConflict is private[getquill]
      //if (CollectAst.byType[Action](subtree).nonEmpty)
      //  AvoidAliasConflict.sanitizeVariables(subtree, dangerousVariables)
      //else
      subtree
  }
}

case class ValParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] with PatMatchDefExt {
  import qctx.tasty.{Type => TType, _}
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(ValDefTerm(ast)) => ast
  }
}

case class BlockParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] with PatMatchDefExt {
  import qctx.tasty.{Type => TType, Block => TBlock, _}
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case block @ Unseal(TBlock(parts, lastPart)) if (parts.length > 0) =>
      val partsAsts =
        parts.map {
          case term: Term => astParse(term.seal)
          case ValDefTerm(ast) => ast
          case other => 
            // TODO Better site-description in error (does other.show work?)
            report.throwError(s"Illegal statement ${other.show} in block ${block.show}")
        }
      val lastPartAst = astParse(lastPart.seal)
      Block((partsAsts :+ lastPartAst))
  }
}

case class CasePatMatchParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] with PatMatchDefExt {
  import qctx.tasty.{Type => TType, _}
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(PatMatchTerm(ast)) => ast
  }
}

trait PatMatchDefExt(implicit val qctx:QuoteContext) extends TastyMatchers {
  import qctx.tasty.{Type => TType, Ident => TIdent, ValDef => TValDef, _}
  import Parser.Implicits._
  import io.getquill.util.Interpolator
  import io.getquill.util.Messages.TraceType
  import io.getquill.norm.BetaReduction

  def astParse: SealedParser[Ast]

  // don't change to ValDef or might override the real valdef in qctx.tasty
  object ValDefTerm {
    def unapply(tree: Tree): Option[Ast] =
      tree match {
        case TValDef(name, Inferred(), Some(t @ PatMatchTerm(ast))) =>
          println(s"====== Parsing Val Def ${name} = ${t.show}")
          Some(Val(Idnt(name), ast))

        // In case a user does a 'def' instead of a 'val' with no paras and no types then treat it as a val def
        // this is useful for things like (TODO Get name) where you'll have something like:
        // query[Person].map(p => (p.name, p.age)).filter(tup => tup._1.name == "Joe")
        // But in Scala3 you can do:
        // query[Person].map(p => (p.name, p.age)).filter((name, age) => name == "Joe")
        // Then in the AST it will look something like:
        // query[Person].map(p => (p.name, p.age)).filter(x$1 => { val name=x$1._1; val age=x$1._2; name == "Joe" })
        // and you need to resolve the val defs thare are created automatically
        case DefDef(name, _, paramss, _, rhsOpt) if (paramss.length == 0) =>
          println(s"====== Parsing Def Def ${name} = ${rhsOpt.map(_.show)}")
          val body =
            rhsOpt match {
              // TODO Better site-description in error
              case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${tree.showExtractors}")
              case Some(rhs) => rhs
            }
          val bodyAst = astParse(body.seal)
          Some(Val(Idnt(name), bodyAst))

        case TValDef(name, Inferred(), rhsOpt) =>
          val body =
            rhsOpt match {
              // TODO Better site-description in error
              case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${tree.showExtractors}")
              case Some(rhs) => rhs
            }
          val bodyAst = astParse(body.seal)
          Some(Val(Idnt(name), bodyAst))

        case _ => None
      }
  }

  object PatMatchTerm {
    def unapply(term: Term): Option[Ast] =
      term match {
        case Match(expr, List(CaseDef(fields, guard, body))) =>
          guard match {
            case Some(guardTerm) => report.throwError("Guards in case- match are not supported", guardTerm.seal)
            case None =>
          }
          Some(patMatchParser(expr, fields, body))

        case other => None
      }
  }
  
  protected def patMatchParser(tupleTree: Term, fieldsTree: Tree, bodyTree: Term): Ast = {
    val tuple = astParse(tupleTree.seal)
    val body = astParse(bodyTree.seal)

    /* 
    Get a list of all the paths of all the identifiers inside the tuple. E.g:
    foo match { case ((a,b),c) => bar } would yield something like:
    List((a,List(_1, _1)), (b,List(_1, _2)), (c,List(_2)))
    */
    def tupleBindsPath(field: Tree, path: List[String] = List()): List[(Idnt, List[String])] = {
      UntypeTree(field) match {
        case Bind(name, TIdent(_)) => List(Idnt(name) -> path)
        case Unapply(Method0(TupleIdent(), "unapply"), something, binds) => 
          binds.zipWithIndex.flatMap { case (bind, idx) =>
            tupleBindsPath(bind, path :+ s"_${idx + 1}")
          }
        case other => report.throwError(s"Invalid Pattern Matching Term: ${other.showExtractors}")
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


// TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
case class QuotationParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Type => TType, Ident => TreeIdent, _}
  import Parser.Implicits._

  // TODO Need to inject this somehow?
  val unlift = new Unlifter()

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    case Apply(TypeApply(Select(TreeIdent("Tuple2"), "apply"), List(Inferred(), Inferred())), List(Select(TreeIdent("p"), "name"), Select(TreeIdent("p"), "age"))) =>
      report.throwError("Matched here!")
    
    case QuotationLotExpr.Unquoted(quotationLot) =>
      quotationLot match {
        case Uprootable(uid, astTree, _, _, _, _) => unlift(astTree)
        case Pluckable(uid, astTree, _) => QuotationTag(uid)
        case Pointable(quote) => report.throwError(s"Quotation is invalid for compile-time or processing: ${quote.show}", quote)
      }

    case ScalarPlanterExpr.UprootableUnquote(expr) =>
      ScalarTag(expr.uid) // TODO Want special scalar tag for an encodeable scalar

    // A inline quotation can be parsed if it is directly inline. If it is not inline, a error
    // must happen (specifically have a check for it or just fail to parse?) 
    // since we would not know the UID since it is not inside of a bin. This situation
    // should only be encountered to a top-level quote passed to the 'run' function and similar situations.
    case QuotedExpr.Uprootable(quotedExpr) => // back here
      unlift(quotedExpr.ast) 
  }
}

case class PropertyAliasParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[PropertyAlias] {
  import qctx.tasty.{Constant => TConstant, _}
  
  def delegate: PartialFunction[Expr[_], PropertyAlias] = {
      case Lambda1(_, '{ ArrowAssoc[$tpa]($prop).->[$v](${ConstExpr(alias: String)}) } ) =>
        def path(tree: Expr[_]): List[String] =
          tree match {
            case a`.`b => 
              path(a) :+ b
            case '{ (${a`.`b}: Option[$t]).map[$r](${Lambda1(arg, body)}) } =>
              path(a) ++ (b :: path(body))
            case _ => 
              Nil
          }

        PropertyAlias(path(prop), alias)
  }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

case class ActionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TConstant, _}
  import Parser.Implicits._

  // TODO If this was copied would 'root' inside of this thing update correctly?
  protected def propertyAliasParser: SealedParser[PropertyAlias] = PropertyAliasParser(root).seal
  
  def delegate: PartialFunction[Expr[_], Ast] = {
    del
  }

  def del: PartialFunction[Expr[_], Ast] = {
    case Unseal(Apply(Select(query, "insert"), insertAssignments)) =>
      val assignments = insertAssignments.filterNot(isNil(_)).map(a => assignmentParser(a))
      Insert(astParse(query.seal), assignments)
  }

  private def isNil(term: Term): Boolean =
    Untype(term) match {
      case Repeated(Nil, any) => true
      case _ => false
    }

  private def assignmentParser(term: Term): Assignment = {
    UntypeExpr(term.seal) match {
      case 
        Lambda1(
          ident,
            Unseal(Apply(TypeApply(
              Select(Apply(
                TypeApply(Ident("ArrowAssoc"), List(Inferred())), 
                List(prop)
              ), "->"), 
              List(Inferred())
            ), List(value))
            )
        ) =>

        Assignment(Idnt(ident), astParse(prop.seal), astParse(value.seal))

      case _ => Parser.throwExpressionError(term.seal, classOf[Assignment])
    }
  }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

case class OptionParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TConstant, Type => TType, _}
  import Parser.Implicits._

  // TODO Move out into TastyMatchers
  def is[T](inputs: Expr[_]*)(implicit test: Type[T]): Boolean =
    inputs.forall(input => input.unseal.tpe <:< test.unseal.tpe)

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ ($o: Option[$t]).isEmpty } => OptionIsEmpty(astParse(o))
    case '{ ($o: Option[$t]).map(${Lambda1(id, body)}) } if (is[Product](o)) => OptionTableMap(astParse(o), Idnt(id), astParse(body))
    case '{ ($o: Option[$t]).map(${Lambda1(id, body)}) } => OptionMap(astParse(o), Idnt(id), astParse(body))
  }
}

case class QueryParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TConstant, _}
  import Parser.Implicits._

  // TODO If this was copied would 'root' inside of this thing update correctly?
  protected def propertyAliasParser: SealedParser[PropertyAlias] = PropertyAliasParser(root).seal
  
  def delegate: PartialFunction[Expr[_], Ast] = {
    del
  }

  def del: PartialFunction[Expr[_], Ast] = {

  // This seems to work?
    case '{ type $t; (new EntityQuery[`$t`]()) } => //: EntityQuery[`$t`]
      val name: String = t.unseal.tpe.classSymbol.get.name
      Entity(name, List())

    case '{ Dsl.querySchema[$t](${ConstExpr(name: String)}, ${GenericSeq(properties)}: _*) } =>
      println("Props are: " + properties.map(_.show))
      val output = Entity.Opinionated(name, properties.toList.map(propertyAliasParser(_)), Renameable.Fixed)
      printer.lnf(output)
      output

    // case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
    //   Map(astParse(q), Idnt("foo"), null)

    //  case q"$query.map[$mt]((x) => y) }"
    case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
      Map(astParse(q), Idnt(ident), astParse(body))

    // Need to have map cases for both Query and EntityQuery since these matches are invariant
    case '{ ($q:EntityQuery[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
      Map(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:EntityQuery[$qt]).flatMap[$mt](${Lambda1(ident, body)}) } => 
      FlatMap(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:Query[$qt]).flatMap[$mt](${Lambda1(ident, body)}) } => 
      FlatMap(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:Query[$qt]).filter(${Lambda1(ident, body)}) } => 
      Filter(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:EntityQuery[$qt]).filter(${Lambda1(ident, body)}) } => 
      Filter(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:Query[$qt]).withFilter(${Lambda1(ident, body)}) } => 
      Filter(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:EntityQuery[$qt]).withFilter(${Lambda1(ident, body)}) } => 
      Filter(astParse(q), Idnt(ident), astParse(body))

    // case Fun(Query(q), "filter", Lambda1(ident, body))

    // Need to have map cases for both Query and EntityQuery since these matches are invariant
    case '{ ($q:EntityQuery[$qt]).filter(${Lambda1(ident, body)}) } => 
      val a = astParse(q)
      val b = astParse(body)
      Filter(a, Idnt(ident), b)

    // TODO Remove duplication once https://github.com/lampepfl/dotty/issues/10464 is fixed
    case '{ ($a: Query[$t]).union($b) } => Union(astParse(a), astParse(b))
    case '{ ($a: EntityQuery[$t]).union($b) } => Union(astParse(a), astParse(b))

    // TODO Remove duplication once https://github.com/lampepfl/dotty/issues/10464 is fixed
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).join[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).leftJoin[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).rightJoin[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).fullJoin[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).join[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).leftJoin[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).rightJoin[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).fullJoin[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).join[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).leftJoin[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).rightJoin[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: EntityQuery[`$t1`]).fullJoin[`$t1`, `$t2`](($q2: Query[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).join[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).leftJoin[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).rightJoin[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type $t1; type $t2; ($q1: Query[`$t1`]).fullJoin[`$t1`, `$t2`](($q2: EntityQuery[`$t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))

    // TODO Remove duplication once https://github.com/lampepfl/dotty/issues/10464 is fixed
    case '{ type $t1; ($q1: EntityQuery[`$t1`]).join[`$t1`](${Lambda1(ident1, on)}) } => 
      FlatJoin(InnerJoin, astParse(q1), Idnt(ident1), astParse(on))
    case '{ type $t1; ($q1: Query[`$t1`]).join[`$t1`](${Lambda1(ident1, on)}) } => 
      FlatJoin(InnerJoin, astParse(q1), Idnt(ident1), astParse(on))
    case '{ type $t1; ($q1: EntityQuery[`$t1`]).leftJoin[`$t1`](${Lambda1(ident1, on)}) } => 
      println(s"========= Parsing Lambda as: ${ident1} => ${on.show} (${astParse(on)}) =========")
      FlatJoin(LeftJoin, astParse(q1), Idnt(ident1), astParse(on))
    case '{ type $t1; ($q1: Query[`$t1`]).leftJoin[`$t1`](${Lambda1(ident1, on)}) } => 
    println(s"========= Parsing Lambda as: ${ident1} => ${on.show} (${astParse(on)}) =========")
      FlatJoin(LeftJoin, astParse(q1), Idnt(ident1), astParse(on))
  }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

// case class ConflictParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: QuoteContext) extends Parser.Clause[Ast] {
//   import qctx.tasty.{Constant => TConstant, using,  _}
//   import Parser.Implicits._
//   def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

//   //  case q"$query.map[$mt]((x) => y) }"
//   //case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 

//   def delegate: PartialFunction[Expr[_], Ast] = {
//     // case q"$query.onConflictIgnore" =>
//     //  OnConflict(astParser(query), OnConflict.NoTarget, OnConflict.Ignore) 
//     case '{ ($query:io.getquill.Insert[$qt]).onConflictIgnore } =>
//       OnConflict(astParse(query), OnConflict.NoTarget, OnConflict.Ignore) 
//   }  
// }

case class OperationsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Type => TType, Ident => TIdent, _}
  import QueryDsl._
  import io.getquill.ast.Infix

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    del.compose(PartialFunction.fromFunction(
      (expr: Expr[_]) => {
        // println(
        //   "==================== Your Expression is: ====================\n" + expr.show
        // )
        // expr match {
        //   case Unseal(Apply(Select(Seal(left), "=="), Seal(right) :: Nil)) =>
        //     println("YAYAYAYAYA WE MATCHED")
        //   case _ =>
        //     println("NOPE DID NOT MATCH")
        // }

        expr
      }
    ))
  }

  object NamedOp1 {
    def unapply(expr: Expr[_]): Option[(Expr[_], String, Expr[_])] =
      UntypeExpr(expr) match {
        case Unseal(Apply(Select(Untype(left), op: String), Untype(right) :: Nil)) => 
          Some(left.seal, op, right.seal)
        case _ => 
          None
      }
  }

  def isType[T](input: Expr[_], test: Type[T]) =
    input.unseal.tpe <:< test.unseal.tpe

  def is[T](inputs: Expr[_]*)(implicit test: Type[T]): Boolean =
    inputs.forall(input => input.unseal.tpe <:< test.unseal.tpe)

  // TODO Is there any way to check if Numeric[T] exists if you don't know the type T
  // but know the type of the term?
  // def isNumeric(expr: Expr[Any]) = {
  //   val tpe = expr.unseal.tpe.seal
  //   Expr.summon(using tpe) match {
  //     case Some(_) => true
  //     case _ => false
  //   }
  // }

  // Function[Expr, Ast]
  def del: PartialFunction[Expr[_], Ast] = {

    case '{ ($str:String).like($other) } => Infix(List(""," like ",""), List(astParse(str), astParse(other)), true)

      // TODO Need to check if entity is a string
    case expr @ NamedOp1(left, "==", right) =>
      val leftAst = astParse(left)
      // left match {
      //   case Unseal(Select(rid @ TIdent(v), prop)) => 
      //   println(s"******* MATCHES ${v} *******")
      //   println(s"Symbol: ${rid.symbol.name}")  
      //   case _ => 
      //     println("********* DOES NOT MATCH *******")
      //     println(left.unseal.showExtractors)
      // }

      val rightAst = astParse(right)
      val res = BinaryOperation(leftAst, EqualityOperator.==, rightAst)
      //println(s"=========== Parsing Equality: ${expr.show} as ${res} ==== WHICH IS: ${leftAst} / ${rightAst} =========")
      res

    case NamedOp1(left, "||", right) =>
      BinaryOperation(astParse(left), BooleanOperator.||, astParse(right))

    case NamedOp1(left, "&&", right) =>
      BinaryOperation(astParse(left), BooleanOperator.&&, astParse(right))

    case '{ ($i: Int).toString } => astParse(i)
    case '{ ($str: String).toInt } => Infix(List("CAST(", " AS Int)"), List(astParse(str)), true)
    case '{ ($str: String).length } => Infix(List("Len(",")"), List(astParse(str)), true)

    case NamedOp1(left, "+", right) if is[String](left) || is[String](right) =>
      BinaryOperation(astParse(left), StringOperator.+, astParse(right))
    
    // 1 + 1
    // Apply(Select(Lit(1), +), Lit(1))
    // Expr[_] => BinaryOperation
    case NumericOperation(binaryOperation) =>
      binaryOperation
  }

  object NumericOperation {
    def unapply(expr: Expr[_]): Option[BinaryOperation] = {
      UntypeExpr(expr) match {
        case NamedOp1(left, NumericOpLabel(binaryOp), right) if (is[Int](left, right)) =>
          Some(BinaryOperation(astParse(left), binaryOp, astParse(right)))
        case _ => None
      }
    }
  }

  object NumericOpLabel {
    def unapply(str: String): Option[BinaryOperator] = 
      str match {
        case "+" => Some(NumericOperator.`+`)
        case "-" => Some(NumericOperator.-)
        case "*" => Some(NumericOperator.*)
        case "/" => Some(NumericOperator./)
        case "%" => Some(NumericOperator.%)
        case ">" => Some(NumericOperator.`>`)
        case "<" => Some(NumericOperator.`<`)
        case _ => None
      }
  }
}



case class GenericExpressionsParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TreeConst, Ident => TreeIdent, _}

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    // Parse tuples
    case Unseal(Apply(TypeApply(Select(TupleIdent(), "apply"), types), values)) =>
      Tuple(values.map(v => astParse(v.seal)))

    //case Unseal(ValDef(name, Inferred(), ) =>

    // TODO Need to figure how how to do with other datatypes
    case Unseal(Literal(TreeConst(v: Double))) => 
      //println("Case Literal Constant")
      Constant(v)

    case Unseal(Literal(TreeConst(v: String))) => 
      //println("Case Literal Constant")
      Constant(v)

    case Unseal(Literal(TreeConst(v: Int))) => 
      //println("Case Literal Constant")
      Constant(v)

    case Unseal(Literal(TreeConst(v: Boolean))) => 
      //println("Case Literal Constant")
      Constant(v)

    case Unseal(value @ Select(Seal(prefix), member)) =>
      if ((value.tpe <:< '[io.getquill.Embedded].unseal.tpe)) { 
        Property.Opinionated(astParse(prefix), member, Renameable.ByStrategy, Visibility.Hidden)
      } else {
        //println(s"========= Parsing Property ${prefix.show}.${member} =========")
        Property(astParse(prefix), member)
      }

    case id @ Unseal(i @ TreeIdent(x)) => 
      val ret = Idnt(i.symbol.name) // TODO How to get decodedName?
      //println(s"====== Parsing Ident ${id.show} as ${ret} ======")
      ret

    // If at the end there's an inner tree that's typed, move inside and try to parse again
    case Unseal(Typed(innerTree, _)) =>
      astParse(innerTree.seal)

    case Unseal(Inlined(_, _, v)) =>
      //println("Case Inlined")
      //root.parse(v.seal.cast[T]) // With method-apply can't rely on it always being T?
      astParse(v.seal)
  }
}
