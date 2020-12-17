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
import miniquill.parser.ParserHelpers._

type Parser[R] = PartialFunction[quoted.Expr[_], R]
type SealedParser[R] = (quoted.Expr[_] => R)

object Parser {
  val empty: Parser[Ast] = PartialFunction.empty[Expr[_], Ast]

  def throwExpressionError(expr: Expr[_], astClass: Class[_])(using Quotes) =
    import quotes.reflect._
    report.throwError(s"""|
      |s"==== Tree cannot be parsed to '${astClass.getSimpleName}' ===
      |  ${Format(expr.show)}
      |==== Extractors ===
      |  ${Format(Printer.TreeStructure.show(expr.asTerm))}
      |==== Tree ===
      |  ${printer.str(expr.asTerm)}
      |""".stripMargin, 
      expr)
  

  object Implicits {

    implicit class ParserExtensions[R](parser: Parser[R])(implicit val qctx: Quotes, ct: ClassTag[R]) {
      import quotes.reflect._

      def seal: SealedParser[R] = 
        (expr: Expr[_]) => parser.lift(expr).getOrElse {
          throwExpressionError(expr, ct.runtimeClass)
        }        
    }
  }

  trait Delegated[R] extends Parser[R] with TastyMatchers {
    implicit val qctx: Quotes
    def delegate: PartialFunction[Expr[_], R]
    override def apply(expr: Expr[_]): R = {
      delegate.apply(expr)
    }
    def isDefinedAt(expr: Expr[_]): Boolean = {
      delegate.isDefinedAt(expr)
    }
  }

  trait Clause[R](using Quotes) extends Delegated[R] with TastyMatchers { base =>
    import Implicits._

    def root: Parser[Ast]
    def astParse = root.seal
    def reparent(root: Parser[Ast]): Clause[R]
  }

  case class Series private (children: List[Clause[Ast]] = List())(override implicit val qctx: Quotes) extends Delegated[Ast] { self =>
    
    def delegate = composite
    
    def composite: PartialFunction[Expr[_], Ast] =
      children.map(child => child.reparent(this)).foldRight(PartialFunction.empty[Expr[_], Ast])(_ orElse _)
    
    def combine(other: Series): Series =
      Series(self.children ++ other.children)
  }

  object Series {
    def single(clause: Clause[Ast])(using Quotes): Series = Series(List(clause))
  }
}

trait ParserFactory {
  def apply(using Quotes): Parser[Ast]
}
trait ParserLibrary extends ParserFactory {
  import Parser._

  def quotationParser(using qctx: Quotes)  =         Series.single(new QuotationParser)
  def queryParser(using qctx: Quotes)      =         Series.single(new QueryParser)
  def patMatchParser(using qctx: Quotes)      =      Series.single(new CasePatMatchParser)
  def functionParser(using qctx: Quotes)   =         Series.single(new FunctionParser)
  def functionApplyParser(using qctx: Quotes) =      Series.single(new FunctionApplyParser)
  def valParser(using qctx: Quotes)      =           Series.single(new ValParser)
  def blockParser(using qctx: Quotes)      =         Series.single(new BlockParser)
  def operationsParser(using qctx: Quotes) =         Series.single(new OperationsParser)
  def genericExpressionsParser(using qctx: Quotes) = Series.single(new GenericExpressionsParser)
  def actionParser(using qctx: Quotes)             = Series.single(new ActionParser)
  def optionParser(using qctx: Quotes)             = Series.single(new OptionParser)

  // def userDefined(using qctxInput: Quotes) = Series(new Glosser[Ast] {
  //   val qctx = qctxInput
  //   def apply(root: Parser[Ast]) = PartialFunction.empty[Expr[_], Ast]
  // })

  def apply(using Quotes): Parser[Ast] = {
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

case class FunctionApplyParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect._
  import Parser.Implicits._
  import io.getquill.norm.capture.AvoidAliasConflict
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  //case q"new { def apply[..t1](...$params) = $body }" =>
  //  c.fail("Anonymous classes aren't supported for function declaration anymore. Use a method with a type parameter instead. " +
  //    "For instance, replace `val q = quote { new { def apply[T](q: Query[T]) = ... } }` by `def q[T] = quote { (q: Query[T] => ... }`")

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(Apply(Select(term, "apply"), args)) =>
      FunctionApply(astParse(term.asExpr), args.map(arg => astParse(arg.asExpr)))
  }
}


case class FunctionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect._
  import Parser.Implicits._
  import io.getquill.norm.capture.AvoidAliasConflict
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  //case q"new { def apply[..t1](...$params) = $body }" =>
  //  c.fail("Anonymous classes aren't supported for function declaration anymore. Use a method with a type parameter instead. " +
  //    "For instance, replace `val q = quote { new { def apply[T](q: Query[T]) = ... } }` by `def q[T] = quote { (q: Query[T] => ... }`")

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(RawLambdaN(params, body)) =>
      val subtree = Function(params.map(Idnt(_)), astParse(body.asExpr))
      // If there are actions inside the subtree, we need to do some additional sanitizations
      // of the variables so that their content will not collide with code that we have generated.

      // TODO Add back once moved to the quill subtree because AvoidAliasConflict is private[getquill]
      //if (CollectAst.byType[Action](subtree).nonEmpty)
      //  AvoidAliasConflict.sanitizeVariables(subtree, dangerousVariables)
      //else
      subtree
  }
}

case class ValParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with PatternMatchingValues {
  import quotes.reflect._
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(ValDefTerm(ast)) => ast
  }
}

case class BlockParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with PatternMatchingValues {
  import quotes.reflect.{Block => TBlock, _}
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case block @ Unseal(TBlock(parts, lastPart)) if (parts.length > 0) =>
      val partsAsts =
        parts.map {
          case term: Term => astParse(term.asExpr)
          case ValDefTerm(ast) => ast
          case other => 
            // TODO Better site-description in error (does other.show work?)
            report.throwError(s"Illegal statement ${other.show} in block ${block.show}")
        }
      val lastPartAst = astParse(lastPart.asExpr)
      Block((partsAsts :+ lastPartAst))
  }
}

case class CasePatMatchParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with PatternMatchingValues {
  import quotes.reflect._
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(PatMatchTerm(ast)) => ast
  }
}



// TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
case class QuotationParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect.{ Ident => TreeIdent, _}
  import Parser.Implicits._

  // TODO Need to inject this somehow?
  val unlift = new Unlifter()

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    // // TODO Document this?
    // case Apply(TypeApply(Select(TreeIdent("Tuple2"), "apply"), List(Inferred(), Inferred())), List(Select(TreeIdent("p"), "name"), Select(TreeIdent("p"), "age"))) =>
    //   report.throwError("Matched here!")
    
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

case class ActionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with Assignments {
  import quotes.reflect.{Constant => TConstant, _}
  import Parser.Implicits._
  
  def delegate: PartialFunction[Expr[_], Ast] = {
    del
  }

  def del: PartialFunction[Expr[_], Ast] = {
    case Unseal(Apply(Select(query, "insert"), insertAssignments)) =>
      val assignments = insertAssignments.filterNot(isNil(_)).map(a => AssignmentTerm.OrFail(a))
      Insert(astParse(query.asExpr), assignments)
  }

  private def isNil(term: Term): Boolean =
    Untype(term) match {
      case Repeated(Nil, any) => true
      case _ => false
    }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

case class OptionParser(root: Parser[Ast] = Parser.empty)(implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import qctx.reflect.{Constant => TConstant, _}
  import Parser.Implicits._

  // TODO Move out into TastyMatchers
  def is[T](inputs: Expr[_]*)(implicit test: Type[T]): Boolean =
    inputs.forall(input => input.asTerm.tpe <:< TypeRepr.of[T])

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ ($o: Option[t]).isEmpty } => OptionIsEmpty(astParse(o))

    case '{ ($o: Option[t]).map(${Lambda1(id, body)}) } => 
      if (is[Product](o)) OptionTableMap(astParse(o), Idnt(id), astParse(body))
      else OptionMap(astParse(o), Idnt(id), astParse(body))
    
    case '{ ($o: Option[t]).exists(${Lambda1(id, body)}) } =>
      if (is[Product](o)) OptionTableExists(astParse(o), Idnt(id), astParse(body))
      else OptionExists(astParse(o), Idnt(id), astParse(body))
  }
}

case class QueryParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with PropertyAliases {
  import qctx.reflect.{Constant => TConstant, _}
  import Parser.Implicits._

  def delegate: PartialFunction[Expr[_], Ast] = {
    del
  }

  def del: PartialFunction[Expr[_], Ast] = {

  // This seems to work?
    case '{ type t; new EntityQuery[`t`]() } => //: EntityQuery[`$t`]
      val name: String = TypeRepr.of[t].classSymbol.get.name
      Entity(name, List())

    case '{ Dsl.querySchema[t](${ConstExpr(name: String)}, ${GenericSeq(properties)}: _*) } =>
      println("Props are: " + properties.map(_.show))
      val output = Entity.Opinionated(name, properties.toList.map(PropertyAliasExpr.OrFail[t](_)), Renameable.Fixed)
      printer.lnf(output)
      output

    case '{ ($q:Query[qt]).map[mt](${Lambda1(ident, body)}) } => 
      Map(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:Query[qt]).flatMap[mt](${Lambda1(ident, body)}) } => 
      FlatMap(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:Query[qt]).filter(${Lambda1(ident, body)}) } => 
      Filter(astParse(q), Idnt(ident), astParse(body))

    case '{ ($q:Query[qt]).withFilter(${Lambda1(ident, body)}) } => 
      Filter(astParse(q), Idnt(ident), astParse(body))

    case '{ ($a: Query[t]).union($b) } => Union(astParse(a), astParse(b))

    case '{ type t1; type t2; ($q1: Query[`t1`]).join[`t1`, `t2`](($q2: Query[`t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type t1; type t2; ($q1: Query[`t1`]).leftJoin[`t1`, `t2`](($q2: Query[`t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type t1; type t2; ($q1: Query[`t1`]).rightJoin[`t1`, `t2`](($q2: Query[`t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))
    case '{ type t1; type t2; ($q1: Query[`t1`]).fullJoin[`t1`, `t2`](($q2: Query[`t2`])).on(${Lambda2(ident1, ident2, on)}) } => Join(InnerJoin, astParse(q1), astParse(q2), Idnt(ident1), Idnt(ident2), astParse(on))

    case '{ type t1; ($q1: Query[`t1`]).join[`t1`](${Lambda1(ident1, on)}) } => 
      FlatJoin(InnerJoin, astParse(q1), Idnt(ident1), astParse(on))
    case '{ type t1; ($q1: Query[`t1`]).leftJoin[`t1`](${Lambda1(ident1, on)}) } => 
      FlatJoin(LeftJoin, astParse(q1), Idnt(ident1), astParse(on))
  }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

// case class ConflictParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
//   import quotes.reflect.{Constant => TConstant, using,  _}
//   import Parser.Implicits._
//   def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

//   //  case q"$query.map[mt]((x) => y) }"
//   //case '{ ($q:Query[qt]).map[mt](${Lambda1(ident, body)}) } => 

//   def delegate: PartialFunction[Expr[_], Ast] = {
//     // case q"$query.onConflictIgnore" =>
//     //  OnConflict(astParser(query), OnConflict.NoTarget, OnConflict.Ignore) 
//     case '{ ($query:io.getquill.Insert[qt]).onConflictIgnore } =>
//       OnConflict(astParse(query), OnConflict.NoTarget, OnConflict.Ignore) 
//   }  
// }

case class OperationsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect._
  import QueryDsl._
  import io.getquill.ast.Infix

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    del.compose(PartialFunction.fromFunction(
      (expr: Expr[_]) => {
        expr
      }
    ))
  }

  object NamedOp1 {
    def unapply(expr: Expr[_]): Option[(Expr[_], String, Expr[_])] =
      UntypeExpr(expr) match {
        case Unseal(Apply(Select(Untype(left), op: String), Untype(right) :: Nil)) => 
          Some(left.asExpr, op, right.asExpr)
        case _ => 
          None
      }
  }

  def isType[T: Type](input: Expr[_]) =
    import quotes.reflect.Term
    input.asTerm.tpe <:< TypeRepr.of[T] // (implicit Type[T])

  def is[T: Type](inputs: Expr[_]*): Boolean =
    import quotes.reflect.Term
    inputs.forall(input => input.asTerm.tpe <:< TypeRepr.of[T])

  // TODO Is there any way to check if Numeric[T] exists if you don't know the type T
  // but know the type of the term?
  // def isNumeric(expr: Expr[Any]) = {
  //   val tpe = expr.unseal.tpe.seal
  //   Expr.summon(using tpe) match {
  //     case Some(_) => true
  //     case _ => false
  //   }
  // }

  def del: PartialFunction[Expr[_], Ast] = {
    case '{ ($str:String).like($other) } => 
      Infix(List(""," like ",""), List(astParse(str), astParse(other)), true)
    case expr @ NamedOp1(left, "==", right) =>
      val leftAst = astParse(left)
      val rightAst = astParse(right)
      val res = BinaryOperation(leftAst, EqualityOperator.==, rightAst)
      res
    case NamedOp1(left, "||", right) =>
      BinaryOperation(astParse(left), BooleanOperator.||, astParse(right))
    case NamedOp1(left, "&&", right) =>
      BinaryOperation(astParse(left), BooleanOperator.&&, astParse(right))

    case '{ ($i: Int).toString } => astParse(i)
    case '{ ($i: String).toString } => astParse(i)
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



case class GenericExpressionsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect.{Constant => TreeConst, Ident => TreeIdent, _}

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    case Unseal(Literal(NullConstant())) =>
      NullValue

    // Parse tuples
    case Unseal(Apply(TypeApply(Select(TupleIdent(), "apply"), types), values)) =>
      Tuple(values.map(v => astParse(v.asExpr)))

    case Unseal(ConstantTerm(v)) => 
      Constant(v)

    case Unseal(value @ Select(Seal(prefix), member)) =>
      if (value.tpe <:< TypeRepr.of[io.getquill.Embedded]) { 
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
      astParse(innerTree.asExpr)

    case Unseal(Inlined(_, _, v)) =>
      //println("Case Inlined")
      //root.parse(v.asExprOf[T]) // With method-apply can't rely on it always being T?
      astParse(v.asExpr)
  }
}
