
package miniquill.parser

import io.getquill.ast.{Ident => Idnt, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.quoted.{Const => ConstExpr, _}
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

  object Implicits {

    implicit class ParserExtensions[R](parser: Parser[R])(implicit val qctx: QuoteContext, ct: ClassTag[R]) {
      import qctx.tasty._

      def seal: SealedParser[R] = 
        (expr: Expr[_]) => parser.lift(expr).getOrElse { //hello
          // c.fail(s"Tree '$tree' can't be parsed to '${ct.runtimeClass.getSimpleName}'")
          report.throwError(
        s"""|s"==== Tree cannot be parsed to '${ct.runtimeClass.getSimpleName}' ===
            |  ${Format(expr.show)}
            |==== Extractors ===
            |  ${Format(expr.unseal.showExtractors)}
            |==== Tree ===
            |  ${printer.str(expr.unseal)}
            |""".stripMargin, 
            expr)
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
  def functionParser(using qctx: QuoteContext)   =         Series.single(new FunctionParser)
  def functionApplyParser(using qctx: QuoteContext) =      Series.single(new FunctionApplyParser)
  def operationsParser(using qctx: QuoteContext) =         Series.single(new OperationsParser)
  def genericExpressionsParser(using qctx: QuoteContext) = Series.single(new GenericExpressionsParser)

  // def userDefined(using qctxInput: QuoteContext) = Series(new Glosser[Ast] {
  //   val qctx = qctxInput
  //   def apply(root: Parser[Ast]) = PartialFunction.empty[Expr[_], Ast]
  // })

  def apply(using qctx: QuoteContext): Parser[Ast] = {
    quotationParser
        .combine(queryParser)
        .combine(functionParser) // decided to have it be it's own parser unlike Quill3
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


// TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
case class QuotationParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Type => TType, _}
  import Parser.Implicits._

  // TODO Need to inject this somehow?
  val unlift = new Unlifter()

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    
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

    // case class Person(name:String)
    // querySchema[Person]("adventureland_people", _.name -> "personName", _.age -> "yrsOld")
    // case q"$.querySchema[$t](${ name: String }, ..$properties)" =>
    case '{ Dsl.querySchema[$t](${ConstExpr(name: String)}, ${GenericSeq(properties)}: _*) } =>
      println("Props are: " + properties.map(_.show))
      val output = Entity.Opinionated(name, properties.toList.map(propertyAliasParser(_)), Renameable.Fixed)
      printer.lnf(output)
      output

    // case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
    //   Map(astParse(q), Idnt("foo"), null)

    //  case q"$query.map[$mt]((x) => y) }"
    case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
      val a = astParse(q)
      val b = astParse(body)
      Map(a, Idnt(ident), b)

    // Need to have map cases for both Query and EntityQuery since these matches are invariant
    case '{ ($q:EntityQuery[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
      val a = astParse(q)
      val b = astParse(body)
      Map(a, Idnt(ident), b)
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
  import qctx.tasty._

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
      del
  }


  def del: PartialFunction[Expr[_], Ast] = {
      // TODO Need to check if entity is a string
    case Unseal(Apply(Select(Seal(left), "+"), Seal(right) :: Nil)) =>
      BinaryOperation(astParse(left), StringOperator.+, astParse(right))

    case Unseal(Apply(Select(Seal(left), "*"), Seal(right) :: Nil)) =>
      BinaryOperation(astParse(left), NumericOperator.*, astParse(right))
  }
}

case class GenericExpressionsParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TreeConst, Ident => TreeIdent, _}

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    // TODO Need to figure how how to do with other datatypes
    case Unseal(Literal(TreeConst(v: Double))) => 
      //println("Case Literal Constant")
      Constant(v)

    case Unseal(Literal(TreeConst(v: String))) => 
      //println("Case Literal Constant")
      Constant(v)

    case Unseal(value @ Select(Seal(prefix), member)) =>
      if ((value.tpe <:< '[io.getquill.Embedded].unseal.tpe)) { 
        Property.Opinionated(astParse(prefix), member, Renameable.ByStrategy, Visibility.Hidden)
      } else {
        Property(astParse(prefix), member)
      }

    case Unseal(TreeIdent(x)) => 
      Idnt(x)

    // If at the end there's an inner tree that's typed, move inside and try to parse again
    case Unseal(Typed(innerTree, _)) =>
      astParse(innerTree.seal)

    case Unseal(Inlined(_, _, v)) =>
      //println("Case Inlined")
      //root.parse(v.seal.cast[T]) // With method-apply can't rely on it always being T?
      astParse(v.seal)
  }
}
