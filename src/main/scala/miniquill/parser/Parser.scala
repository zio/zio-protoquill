
package miniquill.parser

import io.getquill.ast.{Ident => Idnt, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.quoted.matching.{Const => ConstExpr, _}
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable
import miniquill.quoter.QuoteMeta
import scala.reflect.ClassTag


type Parser[R <: Ast] = PartialFunction[Expr[_], R]
object Parser {
  val empty: Parser[Ast] = PartialFunction.empty[Expr[_], Ast]

  trait Delegated[R <: Ast] extends Parser[R] with TastyMatchers {
    implicit val qctx:QuoteContext
    def root: Parser[Ast]
    def delegate: PartialFunction[Expr[_], R]
    def apply(expr: Expr[_]): R = delegate.apply(expr)
    def isDefinedAt(expr: Expr[_]): Boolean = delegate.isDefinedAt(expr)
  }

  trait Clause[R <: Ast](implicit val qctx: QuoteContext) extends Delegated[R] with TastyMatchers { base =>
    def reparent(root: Parser[Ast]): Clause[R]
  }

  class Series(override implicit val qctx: QuoteContext) extends Delegated[Ast] { self =>
    def root = self

    def delegate = composite
  
    def children: List[Clause[Ast]] = List()
    
    def composite: PartialFunction[Expr[_], Ast] =
      children.map(child => child.reparent(this)).foldRight(PartialFunction.empty[Expr[_], Ast])(_ orElse _) //back here
    
    def combine(other: Series): Series =
      new Series { base =>
        override def root = base
        override def children = self.children ++ other.children
      }
  }

  object Series {
    def apply(clause: Clause[Ast])(implicit qctx: QuoteContext): Series =
      new Series {
        override def children = List(clause)
      }
  }
}

trait ParserFactory {
  def apply(given qctx: QuoteContext): Parser[Ast]
}
trait ParserLibrary extends ParserFactory {
  import Parser._

  def quotationParser(given qctx: QuoteContext)  = Series(new QuotationParser)
  def queryParser(given qctx: QuoteContext)      = Series(new QueryParser)
  def operationsParser(given qctx: QuoteContext) = Series(new OperationsParser)
  // def userDefined(given qctxInput: QuoteContext) = Series(new Glosser[Ast] {
  //   val qctx = qctxInput
  //   def apply(root: Parser[Ast]) = PartialFunction.empty[Expr[_], Ast]
  // })
  def genericExpressionsParser(given qctx: QuoteContext) = Series(new GenericExpressionsParser)
  def errorFallbackParser(given qctx: QuoteContext) = Series(new ErrorFallbackParser) 

  def apply(given qctx: QuoteContext): Parser[Ast] =
    quotationParser
        .combine(queryParser)
        .combine(operationsParser)
        // .combine(userDefined)
        .combine(genericExpressionsParser)
        .combine(errorFallbackParser)
}

object ParserLibrary extends ParserLibrary





// TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
case class QuotationParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Type => TType, _, given}

  // TODO Need to inject this somehow?
  val unlift = new Unlifter()

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    
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

// trait PropertyAliasParser extends Glosser[Ast] {
//   implicit val qctx: QuoteContext
//   import qctx.tasty.{Constant => TConstant, given, _}
  
//   def apply(root: Parser[Ast]) = {
//     // def querySchema[T](entity: String, columns: (T => (Any, String))*)
//     // q"(($x1) => $pack.Predef.ArrowAssoc[$t]($prop).$arrow[$v](${ alias: String }))" =>
//     case '{ ($x1: $tpe1) => scala.Predef.ArrowAssoc[$t]($prop).->[$v](${ConstExpr(alias: String)}) } =>
//       Constant("foo")
//   }
// }

case class QueryParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TConstant, given,  _}

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

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

     case '{ ($qm: QuoteMeta[$qt]).querySchema[$t](${ConstExpr(name: String)}, ${ExprSeq(properties)}: _*) } => // back here
        Entity(name, List())

    //     Entity.Opinionated(name, properties.map(propertyAliasParser(_)), Fixed)

    case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
      Map(root(q), Idnt(ident), root(body))

    

    case '{ ($q:Query[$qt]).foobar($v) } => 
      println("=============== We are about to produce: ===============")
      printer.lnf(v.unseal)
      println("================== We are done with the show =============")
      Constant("foobar")
  }
}

case class OperationsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{given, _}

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
      // TODO Need to check if entity is a string
    case Unseal(Apply(Select(Seal(left), "+"), Seal(right) :: Nil)) =>
      BinaryOperation(root(left), StringOperator.+, root(right))

    case Unseal(Apply(Select(Seal(left), "*"), Seal(right) :: Nil)) =>
      BinaryOperation(root(left), NumericOperator.*, root(right))
  }
}

case class GenericExpressionsParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TreeConst, Ident => TreeIdent, given, _}

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
      //println(s"Select ${value.show}")
      //val sealedTpe = value.tpe.seal
      if ((value.tpe <:< '[io.getquill.Embedded].unseal.tpe)) { 
        // TODO Figure how how to check the summon here
        // || (summonExpr(given '[Embedable[$tpee]]).isDefined)
        Property.Opinionated(root(prefix), member, Renameable.ByStrategy, Visibility.Hidden)
      } else {
        Property(root(prefix), member)
      }

    case Unseal(TreeIdent(x)) => 
      Idnt(x)

    // If at the end there's an inner tree that's typed, move inside and try to parse again
    case Unseal(Typed(innerTree, _)) =>
      root(innerTree.seal)

    case Unseal(Inlined(_, _, v)) =>
      //println("Case Inlined")
      //root.parse(v.seal.cast[T]) // With method-apply can't rely on it always being T?
      root(v.seal)
  }
}

case class ErrorFallbackParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{given _, _}

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
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
