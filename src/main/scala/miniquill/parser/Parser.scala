
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


type Parser[R] = PartialFunction[Expr[_], R]
type SealedParser[R] = (Expr[_] => R)

object Parser {
  val empty: Parser[Ast] = PartialFunction.empty[Expr[_], Ast]

  object Implicits {
    implicit class ParserExtensions[R](parser: Parser[R])(implicit val qctx: QuoteContext, ct: ClassTag[R]) {
      import qctx.tasty.{given, _}

      def seal: SealedParser[R] = 
        (expr: Expr[_]) => parser.lift(expr).getOrElse { //hello
          // c.fail(s"Tree '$tree' can't be parsed to '${ct.runtimeClass.getSimpleName}'")
          qctx.throwError(
        s"""|s"==== Tree cannot be parsed to '${ct.runtimeClass.getSimpleName}' ===
            |${expr.show.split("\n").map("  " + _).mkString("\n")}
            |==== Raw ===
            |  ${expr.unseal.showExtractors}
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
    def rootDone = root.seal
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
  def apply(given qctx: QuoteContext): Parser[Ast]
}
trait ParserLibrary extends ParserFactory {
  import Parser._

  def quotationParser(given qctx: QuoteContext)  =         Series.single(new QuotationParser)
  def queryParser(given qctx: QuoteContext)      =         Series.single(new QueryParser)
  def operationsParser(given qctx: QuoteContext) =         Series.single(new OperationsParser)
  def genericExpressionsParser(given qctx: QuoteContext) = Series.single(new GenericExpressionsParser)
  def errorFallbackParser(given qctx: QuoteContext) =      Series.single(new ErrorFallbackParser) 
  // def userDefined(given qctxInput: QuoteContext) = Series(new Glosser[Ast] {
  //   val qctx = qctxInput
  //   def apply(root: Parser[Ast]) = PartialFunction.empty[Expr[_], Ast]
  // })

  def apply(given qctx: QuoteContext): Parser[Ast] = {
    quotationParser
        .combine(queryParser)
        .combine(operationsParser)
        .combine(genericExpressionsParser)
        }
}

object ParserLibrary extends ParserLibrary





// TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
case class QuotationParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx:QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Type => TType, _, given}
  import Parser.Implicits._

  // TODO Need to inject this somehow?
  val unlift = new Unlifter()

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    println("********************************* Trying Quotation Parser **************************")
    del
  }

  def del: PartialFunction[Expr[_], Ast] = {
    
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

case class PropertyAliasParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[PropertyAlias] {
  import qctx.tasty.{Constant => TConstant, given, _}
  
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
  import qctx.tasty.{Constant => TConstant, given,  _}
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


    // TODO can we do this with quoted matching?
    // case 
      
    //   Unseal(
    //     Apply(
    //       TypeApply(
    //         // Need to use showExtractors to get TypeIdentt
    //         Select(New(TypeIdent("EntityQuery")), /* <Init> */ _), _ //List(targ)
    //       ), _
    //     )
        
    //   ) =>
    //   //val name: String = targ.tpe.classSymbol.get.name
    //   Entity("name", List())

    case '{ QuoteDsl.querySchema[$t](${ConstExpr(name: String)}, ${GenericSeq(properties)}: _*) } =>
      println("Props are: " + properties.map(_.show))
      val output = Entity.Opinionated(name, properties.toList.map(propertyAliasParser(_)), Renameable.Fixed)
      printer.lnf(output)
      output

    // case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
    //   //println(e.unseal.showExtractors)
    //   println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GOT TO MAP HERE ^^^^^^^^^^^^^^^^^^^^^^^")
    //   Map(root(q), Idnt("foo"), null)

    //  case q"$query.map[$mt]((x) => y) }"
    case '{ ($q:Query[$qt]).map[$mt](${Lambda1(ident, body)}) } => 
      println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Recurse Query ^^^^^^^^^^^^^^^^^^^^^^^")
      println(q.show)
      val a = this.root.seal(q)
      println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Recurse Body ^^^^^^^^^^^^^^^^^^^^^^^")
      val b = rootDone(body)
      Map(a, Idnt(ident), b)

    case '{ ($q:Query[$qt]).foobar($v) } => 
      printer.lnf(v.unseal)
      Constant("foobar")
  }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

case class OperationsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{given, _}

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
      del
  }


  def del: PartialFunction[Expr[_], Ast] = {
      // TODO Need to check if entity is a string
    case Unseal(Apply(Select(Seal(left), "+"), Seal(right) :: Nil)) =>
      BinaryOperation(rootDone(left), StringOperator.+, rootDone(right))

    case Unseal(Apply(Select(Seal(left), "*"), Seal(right) :: Nil)) =>
      BinaryOperation(rootDone(left), NumericOperator.*, rootDone(right))
  }
}

case class GenericExpressionsParser(root: Parser[Ast] = Parser.empty)(implicit qctx: QuoteContext) extends Parser.Clause[Ast] {
  import qctx.tasty.{Constant => TreeConst, Ident => TreeIdent, given, _}
  val u = printer.xunapplier("Generic Expressions Parser")

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    // TODO Need to figure how how to do with other datatypes
    case Unseal(Literal(TreeConst(v: Double))) => 
      //println("Case Literal Constant")
      Constant(v)

    case Unseal(Literal(TreeConst(v: String))) => 
      //println("Case Literal Constant")
      Constant(v)

    case u(Unseal(value @ Select(Seal(prefix), member))) =>
      if ((value.tpe <:< '[io.getquill.Embedded].unseal.tpe)) { 
        Property.Opinionated(rootDone(prefix), member, Renameable.ByStrategy, Visibility.Hidden)
      } else {
        Property(rootDone(prefix), member)
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
