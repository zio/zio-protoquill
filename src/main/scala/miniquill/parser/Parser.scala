
package miniquill.parser

import io.getquill.ast.{Ident => Idnt, Constant => Const, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.quoted.matching._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable

// TODO change all Expr[_] to Expr[Ast]?
val IdentityParser = PartialFunction.empty[Expr[_], Ast]
trait ParserComponent extends (Parser => PartialFunction[Expr[_], Ast]) with CanSealUnseal {
  def apply(root: Parser): PartialFunction[Expr[_], Ast]  
}

class Parser(given val qctx:QuoteContext) { self =>
  def parse(expr: Expr[_]): Ast = composite.apply(expr)
  def children: List[ParserComponent] = List()
  def composite = children.map(child => child(self)).foldRight(IdentityParser)(_ orElse _)
  def combine(other: Parser): Parser =
    new Parser { base =>
      override def children = self.children ++ other.children
    }
}

object Parser {
  def apply(comp: ParserComponent)(given qctx:QuoteContext): Parser =
    new Parser {
      override def children = List(comp)
    }
}

class BaseParser(given val qctx:QuoteContext) {
  val quotationParser = Parser(new QuotationParser)
  val queryParser = Parser(new QueryParser)
  val operationsParser = Parser(new OperationsParser)
  val genericExpressionsParser = Parser(new GenericExpressionsParser)
  // TODO Maybe tack this on at the very end at the DSL Level
  // Alternatively, separate parsers out into Generic+Error (last two) groups tacked on at the end in Dsl
  // TODO Write a test that adds a custom quotation right in between here
  val errorFallbackParser = Parser(new ErrorFallbackParser) 
  def parser =
    quotationParser
    .combine(queryParser)
    .combine(operationsParser)
    // additional parsing constructs should probably go here. Figure out how to add a better extension point here?
    .combine(genericExpressionsParser)
    .combine(errorFallbackParser)
}

// TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
class QuotationParser(given val qctx:QuoteContext) extends ParserComponent {
  import qctx.tasty.{Type => TType, _, given}

  // TODO Need to inject this somehow?
  val unlift = new Unlifter()

  def apply(root: Parser): PartialFunction[Expr[_], Ast] = {
    
    case QuotationBinExpr.InlineOrPluckedUnquoted(quotationBin) =>
      quotationBin match {
        case InlineableQuotationBinExpr(uid, astTree, _, _) => unlift(astTree)
        case PluckableQuotationBinExpr(uid, astTree) => QuotationTag(uid)
      }

    case ScalarPlanterExpr.InlineUnquote(expr) =>
      ScalarTag(expr.uid) // TODO Want special scalar tag for an encodeable scalar

    // A inline quotation can be parsed if it is directly inline. If it is not inline, a error
    // must happen (specifically have a check for it or just fail to parse?) 
    // since we would not know the UID since it is not inside of a bin. This situation
    // should only be encountered to a top-level quote passed to the 'run' function and similar situations.
    case QuotedExpr.Inline(quotedExpr) => // back here
      unlift(quotedExpr.ast) 
  }
}

class QueryParser(given val qctx: QuoteContext) extends ParserComponent {
  import qctx.tasty.{given, _}

  private object Lambda1 {
    def unapply(term: Term) = term match {
      case Lambda(ValDef(ident, _, _) :: Nil, Seal(methodBody)) => Some((ident, methodBody))
    }
  }

  def apply(root: Parser) = {
    // TODO can we do this with quoted matching?
    case 
      Unseal(
        Apply(
          TypeApply(
            // Need to use showExtractors to get TypeIdentt
            Select(New(TypeIdent("EntityQuery")), /* <Init> */ _), List(targ)
          ), _
        )
      ) =>
      val name: String = targ.tpe.classSymbol.get.name
      Entity(name, List())

    case vv @ '{ ($q:Query[$qt]).map[$mt](${Unseal(Lambda1(ident, body))}) } => 
      Map(root.parse(q), Idnt(ident), root.parse(body))
  }
}

class OperationsParser(given val qctx: QuoteContext) extends ParserComponent {
  import qctx.tasty.{given, _}

  def apply(root: Parser) = {
      // TODO Need to check if entity is a string
    case Unseal(Apply(Select(Seal(left), "+"), Seal(right) :: Nil)) =>
      BinaryOperation(root.parse(left), StringOperator.+, root.parse(right))

    case Unseal(Apply(Select(Seal(left), "*"), Seal(right) :: Nil)) =>
      BinaryOperation(root.parse(left), NumericOperator.*, root.parse(right))
  }
}

class GenericExpressionsParser(given val qctx: QuoteContext) extends ParserComponent {
  import qctx.tasty.{given, _}

  def apply(root: Parser) = {

    // TODO Need to figure how how to do with other datatypes
    case Unseal(Literal(Constant(v: Double))) => 
      //println("Case Literal Constant")
      Const(v)

    case Unseal(Literal(Constant(v: String))) => 
      //println("Case Literal Constant")
      Const(v)

    case Unseal(value @ Select(Seal(prefix), member)) =>
      //println(s"Select ${value.show}")
      //val sealedTpe = value.tpe.seal
      if ((value.tpe <:< '[io.getquill.Embedded].unseal.tpe)) { 
        // TODO Figure how how to check the summon here
        // || (summonExpr(given '[Embedable[$tpee]]).isDefined)
        Property.Opinionated(root.parse(prefix), member, Renameable.ByStrategy, Visibility.Hidden)
      } else {
        Property(root.parse(prefix), member)
      }

    case Unseal(Ident(x)) => 
      //println("Ident")
      Idnt(x)

    // If at the end there's an inner tree that's typed, move inside and try to parse again
    case Unseal(Typed(innerTree, _)) =>
      root.parse(innerTree.seal)

    case Unseal(Inlined(_, _, v)) =>
      //println("Case Inlined")
      //root.parse(v.seal.cast[T]) // With method-apply can't rely on it always being T?
      root.parse(v.seal)
  }
}

class ErrorFallbackParser(given val qctx: QuoteContext) extends ParserComponent {
  import qctx.tasty.{given, _}

  def apply(root: Parser): PartialFunction[Expr[_], Ast] = {
    // TODO define this a last-resort printing function inside the parser
    case Unseal(t) =>
      println("=============== Parsing Error ================\n" + printer.ln(t))
      println("=============== Extracted ================\n" + t.showExtractors)
      ???
      //println(t)
      //summon[QuoteContext].error("Parsing error: " + in.show, in)
      //???
  }
}
