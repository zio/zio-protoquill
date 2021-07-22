package io.getquill.parser

import io.getquill.ast.{Ident => AIdent, Query => AQuery, Action => AAction, Insert => AInsert, Update => AUpdate, Delete => ADelete, _}
import io.getquill.ast
import io.getquill.metaprog.PlanterExpr
import io.getquill.metaprog.QuotedExpr
import scala.quoted._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable
import scala.reflect.ClassTag
import io.getquill.norm.capture.AvoidAliasConflict
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.util.Format
import io.getquill.parser.ParserHelpers._
import io.getquill.quat.QuatMaking
import io.getquill.quat.QuatMakingBase
import io.getquill.quat.Quat
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.Uprootable
import io.getquill.metaprog.Pluckable
import io.getquill.metaprog.Pointable
import io.getquill.metaprog.Extractors._
import io.getquill.util.printer
import io.getquill._
import io.getquill.Ord
import io.getquill.Embedded
import io.getquill.metaprog.Is
import io.getquill.generic.ElaborationSide

type Parser[R] = PartialFunction[quoted.Expr[_], R]
type SealedParser[R] = (quoted.Expr[_] => R)

/**
  * Parse Quill-DSL expressions into the Quill AST.
  *
  * Naming Legend:
  *  - AIdent/AQuery/etc... means Quill Ast-Ident/Query...
  *  - TIdent/TConstant/ec... means Tasty Ident/Constant/etc...
  */
object Parser {
  val empty: Parser[Ast] = PartialFunction.empty[Expr[_], Ast]

  enum ThrowInfo:
    case AstClass(astClass: Class[_])
    case Message(msg: String)

  def throwExpressionError(expr: Expr[_], astClass: Class[_])(using Quotes): Nothing =
    throwExpressionError(expr, ThrowInfo.AstClass(astClass))

  def throwExpressionError(expr: Expr[_], msg: String)(using Quotes): Nothing =
    throwExpressionError(expr, ThrowInfo.Message(msg))

  def throwExpressionError(expr: Expr[_], throwInfo: ThrowInfo)(using Quotes): Nothing =
    import quotes.reflect._
    // When errors are printed, make sure to deserialize parts of the AST that may be serialized,
    // otherwise in the expression printout there will garbled base46 characters everywhere
    val term = io.getquill.metaprog.DeserializeAstInstances(expr).asTerm
    val message =
      throwInfo match
        case ThrowInfo.Message(msg) => msg
        case ThrowInfo.AstClass(astClass) => s"Tree cannot be parsed to '${astClass.getSimpleName}'"
    report.throwError(s"""|
      |s"==== ${message} ====
      |  ${Format(Printer.TreeShortCode.show(term)) /* Or Maybe just expr? */}
      |==== Extractors ===
      |  ${Format(Printer.TreeStructure.show(term))}
      |""".stripMargin,
      expr
    )

  object Implicits {
    extension [R](parser: Parser[R])(using ct: ClassTag[R])
      def seal(using Quotes): SealedParser[R] =
        (expr: Expr[_]) => parser.lift(expr).getOrElse {
          throwExpressionError(expr, ct.runtimeClass)
        }
  }

  trait Delegated[+R] extends Parser[R] {
    def delegate: PartialFunction[Expr[_], R]

    //apply() is an abstract member of PartialFunction
    override def apply(expr: Expr[_]): R = {
      delegate.apply(expr)
    }

    //isDefinedAt() is an abstract member of PartialFunction
    //Checks if a value is contained in the function's domain.
    def isDefinedAt(expr: Expr[_]): Boolean = {
      delegate.isDefinedAt(expr)
    }

  }

  /** Optimizes 'Clause' by checking if it is some given type first. Otherwise can early-exit */
  trait SpecificClause[Criteria: Type, R](using override val qctx: Quotes) extends Clause[R] {
    import quotes.reflect._
    override def isDefinedAt(expr: Expr[_]): Boolean =
      expr.asTerm.tpe <:< TypeRepr.of[Criteria] && delegate.isDefinedAt(expr)
  }

  /** Optimizes 'Clause' by allowing a more efficient 'prematch' criteria to be used */
  trait PrematchClause[R](using override val qctx: Quotes) extends Clause[R] {
    import qctx.reflect._

    def prematch(expr: Expr[_]): Boolean

    override def isDefinedAt(expr: Expr[_]): Boolean =
      prematch(expr) && delegate.isDefinedAt(expr)
  }

  abstract class Clause[+R](using override val qctx: Quotes) extends Delegated[R] with Idents with QuatMaking with QuatMakingBase(using qctx) { base =>
    import Implicits._

    def root: Parser[Ast]
    def astParse = root.seal
    def reparent(root: Parser[Ast]): Clause[R]
  }

  case class Series private (children: List[Clause[Ast]] = List())(implicit val qctx: Quotes) extends Delegated[Ast] { self =>

    def delegate = composite

    def composite: PartialFunction[Expr[_], Ast] =
      children.map(child => child.reparent(this)).foldRight(PartialFunction.empty[Expr[_], Ast])(_ orElse _)

    //Is this to chain parsers?
    def combine(other: Series): Series =
      Series(self.children ++ other.children)
  }

  object Series {
    def single(clause: Clause[Ast])(using Quotes): Series = Series(List(clause))
    def of(clauses: Clause[Ast]*)(using Quotes) = Series(clauses.toList)
  }
}

trait ParserFactory {
  def apply(using Quotes): Parser[Ast]
}
trait ParserLibrary extends ParserFactory {
  import Parser._


  // TODO add a before everything identity parser,
  // a after everything except Inline recurse parser
  def quotationParser(using qctx: Quotes)            = Series.single(new QuotationParser)
  def queryParser(using qctx: Quotes)                = Series.single(new QueryParser)
  def infixParser(using qctx: Quotes)                = Series.single(new InfixParser)
  def setOperationsParser(using qctx: Quotes)        = Series.single(new SetOperationsParser)
  def queryScalarsParser(using qctx: Quotes)         = Series.single(new QueryScalarsParser)
  def traversableOperationParser(using qctx: Quotes) = Series.single(new TraversableOperationParser)
  def patMatchParser(using qctx: Quotes)             = Series.single(new CasePatMatchParser)
  def functionParser(using qctx: Quotes)             = Series.single(new FunctionParser)
  def functionApplyParser(using qctx: Quotes)        = Series.single(new FunctionApplyParser)
  def valParser(using qctx: Quotes)                  = Series.single(new ValParser)
  def blockParser(using qctx: Quotes)                = Series.single(new BlockParser)
  def operationsParser(using qctx: Quotes)           = Series.single(new OperationsParser)
  def orderingParser(using qctx: Quotes)             = Series.single(new OrderingParser)
  def genericExpressionsParser(using qctx: Quotes)   = Series.single(new GenericExpressionsParser)
  def actionParser(using qctx: Quotes)               = Series.single(new ActionParser)
  def batchActionParser(using qctx: Quotes)          = Series.single(new BatchActionParser)
  def optionParser(using qctx: Quotes)               = Series.single(new OptionParser)
  def ifElseParser(using qctx: Quotes)               = Series.single(new IfElseParser)
  def complexValueParser(using qctx: Quotes)         = Series.single(new ComplexValueParser)
  def valueParser(using qctx: Quotes)                = Series.single(new ValueParser)

  // def userDefined(using qctxInput: Quotes) = Series(new Glosser[Ast] {
  //   val qctx = qctxInput
  //   def apply(root: Parser[Ast]) = PartialFunction.empty[Expr[_], Ast]
  // })

  //Everything needs to be parsed from a quoted state, and sent to the subsequent parser
  def apply(using Quotes): Parser[Ast] =
    quotationParser
        .combine(valueParser)
        .combine(queryParser)
        .combine(queryScalarsParser)
        .combine(infixParser)
        .combine(setOperationsParser)
        .combine(traversableOperationParser)
        .combine(optionParser)
        .combine(orderingParser)
        .combine(actionParser)
        .combine(batchActionParser)
        .combine(functionParser) // decided to have it be it's own parser unlike Quill3
        .combine(patMatchParser)
        .combine(valParser)
        .combine(blockParser)
        .combine(operationsParser)
        .combine(ifElseParser)
        .combine(complexValueParser) // must go before functionApplyParser since valueParser parsers '.apply on case class' and the functionApply would take that
        .combine(functionApplyParser) // must go before genericExpressionsParser otherwise that will consume the 'apply' clauses
        .combine(genericExpressionsParser)
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


case class FunctionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.PrematchClause[Ast] {
  import quotes.reflect._
  import Parser.Implicits._
  import io.getquill.norm.capture.AvoidAliasConflict
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  // TODO Find out what is type signature of things that need to go into here
  // maybe the type just needs to be widened?
  def prematch(expr: Expr[_]) = true //expr.asTerm.tpe.typeSymbol.fullName.contains("scala.Function")

  //case q"new { def apply[..t1](...$params) = $body }" =>
  //  c.fail("Anonymous classes aren't supported for function declaration anymore. Use a method with a type parameter instead. " +
  //    "For instance, replace `val q = quote { new { def apply[T](q: Query[T]) = ... } }` by `def q[T] = quote { (q: Query[T] => ... }`")

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(RawLambdaN(params, body)) =>
      val subtree = Function(params.map((name, tpe) => cleanIdent(name, tpe)), astParse(body.asExpr))
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
  import quotes.reflect.{ Constant => TConstant, _ }
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case Unseal(PatMatchTerm(patMatch)) =>
      patMatch match
        case PatMatch.SimpleClause(ast) => ast
        case PatMatch.MultiClause(clauses: List[PatMatchClause]) => nestedIfs(clauses)
        case PatMatch.AutoAddedTrivialClause => Constant(true, Quat.BooleanValue)
  }

  def nestedIfs(clauses: List[PatMatchClause]): Ast =
    clauses match
      case PatMatchClause(body, guard) :: tail => ast.If(guard, body, nestedIfs(tail))
      case Nil => ast.NullValue
}

/** Same as traversableOperationParser, pre-filters that the result-type is a boolean */
case class TraversableOperationParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.SpecificClause[Boolean, Ast] with PatternMatchingValues {
  import quotes.reflect._
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ ($col: collection.Map[k, v]).contains($body) } =>
      MapContains(astParse(col), astParse(body))
    case '{ ($col: collection.Set[v]).contains($body) } =>
      SetContains(astParse(col), astParse(body))
    case '{ ($col: collection.Seq[v]).contains($body) } =>
      ListContains(astParse(col), astParse(body))
  }
}


case class OrderingParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[io.getquill.ast.Ordering] with PatternMatchingValues {
  import quotes.reflect._
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ordering] = {
    case '{ implicitOrd } => AscNullsFirst

    // Doing this on a lower level since there are multiple cases of Order.apply with multiple arguemnts
    case Unseal(Apply(TypeApply(Select(Ident("Ord"), "apply"), _), args)) =>
      // parse all sub-orderings if this is a composite
      val subOrderings = args.map(_.asExpr).map(ordExpression => delegate.seal(ordExpression))
      TupleOrdering(subOrderings)

    case '{ Ord.asc[t] }               => Asc
    case '{ Ord.desc[t] }              => Desc
    case '{ Ord.ascNullsFirst[t] }     => AscNullsFirst
    case '{ Ord.descNullsFirst[t] }    => DescNullsFirst
    case '{ Ord.ascNullsLast[t] }      => AscNullsLast
    case '{ Ord.descNullsLast[t] }     => DescNullsLast
  }
}


// TODO Pluggable-in unlifter via implicit? Quotation generic should have it in the root?
case class QuotationParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect.{ Ident => TIdent, _}
  import Parser.Implicits._

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    // // TODO Document this?
    // case Apply(TypeApply(Select(TIdent("Tuple2"), "apply"), List(Inferred(), Inferred())), List(Select(TIdent("p"), "name"), Select(TIdent("p"), "age"))) =>
    //   report.throwError("Matched here!")

    case QuotationLotExpr.Unquoted(quotationLot) =>
      quotationLot match {
        case Uprootable(uid, astTree, _) => Unlifter(astTree)
        case Pluckable(uid, astTree, _) => QuotationTag(uid)
        case Pointable(quote) => report.throwError(s"Quotation is invalid for compile-time or processing: ${quote.show}", quote)
      }

    case PlanterExpr.UprootableUnquote(expr) =>
      ScalarTag(expr.uid) // TODO Want special scalar tag for an encodeable scalar

    // A inline quotation can be parsed if it is directly inline. If it is not inline, a error
    // must happen (specifically have a check for it or just fail to parse?)
    // since we would not know the UID since it is not inside of a bin. This situation
    // should only be encountered to a top-level quote passed to the 'run' function and similar situations.
    // NOTE: Technically we only need to uproot the AST here, not the lifts, but using UprootableWithLifts
    // since that's the only construct in QuotedExpr that checks if there is an Inline block at the front.
    // If needed for performance reasons, a Inlined checking extractor can be made in QuotedExpr that ignores lifts
    case QuotedExpr.UprootableWithLifts(quotedExpr, _) =>
      Unlifter(quotedExpr.ast)
  }
}

// As a performance optimization, ONLY Matches things returning Action[_] UP FRONT.
// All other kinds of things rejected
case class ActionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.SpecificClause[Action[_], Ast] with Assignments with PropertyParser {
  import quotes.reflect.{Constant => TConstant, _}
  import Parser.Implicits._

  def delegate: PartialFunction[Expr[_], Ast] = {
    del
  }

  def combineAndCheckAndParse[T: Type, A <: Ast](first: Expr[T], others: Seq[Expr[T]])(checkClause: Expr[_] => Unit)(parseClause: Expr[_] => A): Seq[A] =
    val assignments = (first.asTerm +: others.map(_.asTerm).filterNot(isNil(_)))
    assignments.foreach(term => checkClause(term.asExpr))
    assignments.map(term => parseClause(term.asExpr))


  def del: PartialFunction[Expr[_], Ast] = {
    case '{ type t; ($query: EntityQueryModel[`t`]).insert(($first: `t`=>(Any,Any)), (${Varargs(others)}: Seq[`t` => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(first, others)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.OrFail(_))
      AInsert(astParse(query), assignments.toList)
    case '{ type t; ($query: EntityQueryModel[`t`]).update(($first: `t`=>(Any,Any)), (${Varargs(others)}: Seq[`t` => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(first, others)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.OrFail(_))
      AUpdate(astParse(query), assignments.toList)
    case '{ type t; ($query: EntityQueryModel[`t`]).delete } =>
      ADelete(astParse(query))

    // Returning generated
    // summon an idiom if one can be summoned (make sure when doing import ctx._ it is provided somewhere)
    // (for dialect-specific behavior, try to summon a dialect implicitly, it may not exist since a dialect may
    // not be supported. If one exists then check the type of returning thing. If not then dont.
    // Later: Introduce a module that verifies the AST later before compilation and emits a warning if the returning type is incorrect
    //        do the same thing for dynamic contexts witha log message

    // Form:    ( (Query[Perosn]).[action](....) ).returning[T]
    // Example: ( query[Person].insert(lift(joe))).returning[Something]
    case '{ ($action: Insert[t]).returning[r] } =>
      report.throwError(s"A 'returning' clause must have arguments.")
    // NOTE: Need to make copies for Insert/Update/Delete because presently `Action` does not have a .returning method
    case '{ ($action: Update[t]).returning[r] } =>
      report.throwError(s"A 'returning' clause must have arguments.")
    case '{ ($action: Delete[t]).returning[r] } =>
      report.throwError(s"A 'returning' clause must have arguments.")

    case '{ ($action: Insert[t]).returning[r](${Lambda1(id, tpe, body)}) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, astParse(body), action, Type.of[t])
      // // TODO Verify that the idiom supports this type of returning clause
      // idiomReturnCapability match {
      //   case ReturningMultipleFieldSupported | ReturningClauseSupported | OutputClauseSupported =>
      //   case ReturningSingleFieldSupported =>
      //     c.fail(s"The 'returning' clause is not supported by the ${currentIdiom.getOrElse("specified")} idiom. Use 'returningGenerated' instead.")
      //   case ReturningNotSupported =>
      //     c.fail(s"The 'returning' or 'returningGenerated' clauses are not supported by the ${currentIdiom.getOrElse("specified")} idiom.")
      // }
      // // Verify that the AST in the returning-body is valid
      // idiomReturnCapability.verifyAst(bodyAst)
      Returning(astParse(action), ident, bodyAst)

    case '{ ($action: Insert[t]).onConflictIgnore } =>
      OnConflict(astParse(action), OnConflict.NoTarget, OnConflict.Ignore)

    case '{ type t; ($action: Insert[`t`]).onConflictIgnore(($target: `t` => Any), (${Varargs(targets)}: Seq[`t` => Any]): _*) } =>
      val targetProperties = combineAndCheckAndParse(target, targets)(_ => ())(LambdaToProperty.OrFail(_))
      OnConflict(
        astParse(action),
        OnConflict.Properties(targetProperties.toList),
        OnConflict.Ignore
      )

    case '{ type t; ($action: Insert[`t`]).onConflictUpdate(($assign: (`t`,`t`) => (Any, Any)), (${Varargs(assigns)}: Seq[(`t`, `t`) => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(assign, assigns)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.Double.OrFail(_))
      OnConflict(
        astParse(action),
        OnConflict.NoTarget,
        OnConflict.Update(assignments.toList)
      )

    case '{ type t; ($action: Insert[`t`]).onConflictUpdate(($target: `t` => Any), (${Varargs(targets)}: Seq[`t` => Any]): _*)(($assign: (`t`,`t`) => (Any, Any)), (${Varargs(assigns)}: Seq[(`t`, `t`) => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(assign, assigns)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.Double.OrFail(_))
      val targetProperties = combineAndCheckAndParse(target, targets)(_ => ())(LambdaToProperty.OrFail(_))
      OnConflict(
        astParse(action),
        OnConflict.Properties(targetProperties.toList),
        OnConflict.Update(assignments.toList)
      )

    // Need to make copies because presently `Action` does not have a .returning method
    case '{ ($action: Update[t]).returning[r](${Lambda1(id, tpe, body)}) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, astParse(body), action, Type.of[t])
      Returning(astParse(action), ident, bodyAst)
    case '{ ($action: Delete[t]).returning[r](${Lambda1(id, tpe, body)}) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, astParse(body), action, Type.of[t])
      Returning(astParse(action), ident, bodyAst)

    case '{ ($action: Insert[t]).returningGenerated[r](${Lambda1(id, tpe, body)}) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, astParse(body), action, Type.of[t])
      // // TODO Verify that the idiom supports this type of returning clause
      // idiomReturnCapability match {
      //   case ReturningNotSupported =>
      //     c.fail(s"The 'returning' or 'returningGenerated' clauses are not supported by the ${currentIdiom.getOrElse("specified")} idiom.")
      //   case _ =>
      // }
      // // Verify that the AST in the returning-body is valid
      // idiomReturnCapability.verifyAst(bodyAst)
      ReturningGenerated(astParse(action), ident, bodyAst)
  }

  private def isNil(term: Term): Boolean =
    Untype(term) match {
      case Repeated(Nil, any) => true
      case _ => false
    }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  import io.getquill.generic.ElaborateStructure

  /**
   * Note. Ported from reprocessReturnClause in Scala2-Quill parsing. Logic is much simpler in Scala 3
   * and potentially can be simplified even further.
   * In situations where the a `.returning` clause returns the initial record i.e. `.returning(r => r)`,
   * we need to expand out the record into it's fields i.e. `.returning(r => (r.foo, r.bar))`
   * otherwise the tokenizer would be force to pass `RETURNING *` to the SQL which is a problem
   * because the fields inside of the record could arrive out of order in the result set
   * (e.g. arrive as `r.bar, r.foo`). Use use the value/flatten methods in order to expand
   * the case-class out into fields.
   */
  private def reprocessReturnClause(ident: AIdent, originalBody: Ast, action: Expr[_], actionType: Type[_]) =
    (ident == originalBody, action.asTerm.tpe) match
      case (true , IsActionType()) =>
        val newBody =
          actionType match
            case '[at] => ElaborateStructure.ofAribtraryType[at](ident.name, ElaborationSide.Decoding) // elaboration side is Decoding since this is for entity being returned from the Quill query
        newBody
      case (true, _) =>
        report.throwError("Could not process whole-record 'returning' clause. Consider trying to return individual columns.")
      case _ =>
        originalBody

  object IsActionType {
    def unapply(term: TypeRepr): Boolean =
      term <:< TypeRepr.of[Insert[_]] || term <:< TypeRepr.of[Update[_]] || term <:< TypeRepr.of[Delete[_]]
  }
}

// As a performance optimization, ONLY Matches things returning BatchAction[_] UP FRONT.
// All other kinds of things rejected
case class BatchActionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.SpecificClause[BatchAction[_], Ast] with Assignments {
  import quotes.reflect.{Constant => TConstant, _}
  import Parser.Implicits._

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ type a <: Action[_] with QAC[_, _]; ($q: Query[t]).foreach[`a`, b](${Lambda1(ident, tpe, body)})($unq) } =>
      Foreach(astParse(q), cleanIdent(ident, tpe), astParse(body))
  }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

case class IfElseParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import qctx.reflect.{Constant => TConstant, _}
  import Parser.Implicits._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] =
    case '{ if ($cond) { $thenClause } else { $elseClause } } =>
      ast.If(astParse(cond), astParse(thenClause), astParse(elseClause))
}

// We can't use SpecificClause[Option[_]] here since the types of quotations that need to match
// are not necessarily an Option[_] e.g. Option[t].isEmpty needs to match on a clause whose type is Boolean
// That's why we need to use the 'Is' object and optimize it that way here
case class OptionParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import qctx.reflect.{Constant => TConstant, _}
  import Parser.Implicits._
  import MatchingOptimizers._

  extension (quat: Quat)
    def isProduct = quat.isInstanceOf[Quat.Product]

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  /**
   * Note: The -->, -@> etc.. clauses are just to optimize the match by doing an early-exit if possible.
   * they don't actaully do any application-relevant logic
   */
  def delegate: PartialFunction[Expr[_], Ast] = {
    case "isEmpty" --> '{ ($o: Option[t]).isEmpty } =>
      OptionIsEmpty(astParse(o))

    case "nonEmpty" --> '{ ($o: Option[t]).nonEmpty } =>
      OptionNonEmpty(astParse(o))

    case "isDefined" --> '{ ($o: Option[t]).isDefined } =>
      OptionIsDefined(astParse(o))

    case "flatten" -@> '{ ($o: Option[t]).flatten($impl) } =>
      OptionFlatten(astParse(o))

    case "map" -@> '{ ($o: Option[t]).map(${Lambda1(id, idType, body)}) } =>
      val queryAst = astParse(o)
      if (queryAst.quat.isProduct) OptionTableMap(astParse(o), cleanIdent(id, idType), astParse(body))
      else OptionMap(queryAst, cleanIdent(id, idType), astParse(body))

    case "flatMap" -@> '{ ($o: Option[t]).flatMap(${Lambda1(id, idType, body)}) } =>
      val queryAst = astParse(o)
      if (queryAst.quat.isProduct) OptionTableFlatMap(astParse(o), cleanIdent(id, idType), astParse(body))
      else OptionFlatMap(queryAst, cleanIdent(id, idType), astParse(body))

    case "exists" -@> '{ ($o: Option[t]).exists(${Lambda1(id, idType, body)}) } =>
      val queryAst = astParse(o)
      if (queryAst.quat.isProduct) OptionTableExists(astParse(o), cleanIdent(id, idType), astParse(body))
      else OptionExists(queryAst, cleanIdent(id, idType), astParse(body))

    case "forall" -@> '{ ($o: Option[t]).forall(${Lambda1(id, idType, body)}) } =>
      // TODO If is embedded warn about no checking? Investigate that case
      val queryAst = astParse(o)
      if (queryAst.quat.isProduct) OptionTableForall(astParse(o), cleanIdent(id, idType), astParse(body))
      else OptionForall(queryAst, cleanIdent(id, idType), astParse(body))

    case "getOrElse" -@> '{ type t; ($o: Option[`t`]).getOrElse($body: `t`) } =>
      OptionGetOrElse(astParse(o), astParse(body))

    case "contains" -@> '{ type t; ($o: Option[`t`]).contains($body: `t`) } =>
      OptionContains(astParse(o), astParse(body))
  }
}

// As a performance optimization, ONLY Matches things returning Query[_] UP FRONT.
// All other kinds of things rejected
case class QueryParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.SpecificClause[Query[_], Ast] with PropertyAliases {
  import qctx.reflect.{Constant => TConstant, Ident => TIdent, _}
  import Parser.Implicits._
  import MatchingOptimizers._

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ type t; EntityQuery.apply[`t`] } =>
      val tpe = TypeRepr.of[t]
      val name: String = tpe.classSymbol.get.name
        Entity(name, List(), InferQuat.ofType(tpe).probit)

    case '{ querySchema[t](${ConstExpr(name: String)}, ${GenericSeq(properties)}: _*) } =>
      val e: Entity = Entity.Opinionated(name, properties.toList.map(PropertyAliasExpr.OrFail[t](_)), InferQuat.of[t].probit, Renameable.Fixed)
      e

    case "map" -@> '{ ($q:Query[qt]).map[mt](${Lambda1(ident, tpe, body)}) } =>
      Map(astParse(q), cleanIdent(ident, tpe), astParse(body))

    case "flatMap" -@> '{ ($q:Query[qt]).flatMap[mt](${Lambda1(ident, tpe, body)}) } =>
      FlatMap(astParse(q), cleanIdent(ident, tpe), astParse(body))

    case "filter" -@> '{ ($q:Query[qt]).filter(${Lambda1(ident, tpe, body)}) } =>
      Filter(astParse(q), cleanIdent(ident, tpe), astParse(body))

    case "withFilter" -@> '{ ($q:Query[qt]).withFilter(${Lambda1(ident, tpe, body)}) } =>
      Filter(astParse(q), cleanIdent(ident, tpe), astParse(body))

    case "concatMap" -@@> '{type t1; type t2; ($q:Query[qt]).concatMap[`t1`, `t2`](${Lambda1(ident, tpe, body)})($unknown_stuff) } => //ask Alex why is concatMap like this? what's unkonwn_stuff?
      ConcatMap(astParse(q), cleanIdent(ident, tpe), astParse(body))

    case "union"    -@> '{ ($a: Query[t]).union($b) } => Union(astParse(a), astParse(b))
    case "unionAll" -@> '{ ($a: Query[t]).unionAll($b) } => UnionAll(astParse(a), astParse(b))
    case "++"       -@> '{ ($a: Query[t]).++($b) } => UnionAll(astParse(a), astParse(b))

    case ("join"      -@> '{ type t1; type t2; ($q1: Query[`t1`]).join[`t1`, `t2`](($q2: Query[`t2`])) })      withOnClause  (OnClause(ident1, tpe1, ident2, tpe2, on)) => Join(InnerJoin, astParse(q1), astParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), astParse(on))
    case ("leftJoin"  -@> '{ type t1; type t2; ($q1: Query[`t1`]).leftJoin[`t1`, `t2`](($q2: Query[`t2`])) })  withOnClause  (OnClause(ident1, tpe1, ident2, tpe2, on)) => Join(LeftJoin, astParse(q1), astParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), astParse(on))
    case ("rightJoin" -@> '{ type t1; type t2; ($q1: Query[`t1`]).rightJoin[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause  (OnClause(ident1, tpe1, ident2, tpe2, on)) => Join(RightJoin, astParse(q1), astParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), astParse(on))
    case ("fullJoin"  -@> '{ type t1; type t2; ($q1: Query[`t1`]).fullJoin[`t1`, `t2`](($q2: Query[`t2`])) })  withOnClause  (OnClause(ident1, tpe1, ident2, tpe2, on)) => Join(FullJoin, astParse(q1), astParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), astParse(on))

    case "join" -@> '{ type t1; ($q1: Query[`t1`]).join[`t1`](${Lambda1(ident1, tpe, on)}) } =>
      FlatJoin(InnerJoin, astParse(q1), cleanIdent(ident1, tpe), astParse(on))
    case "leftJoin" -@> '{ type t1; ($q1: Query[`t1`]).leftJoin[`t1`](${Lambda1(ident1, tpe, on)}) } =>
      FlatJoin(LeftJoin, astParse(q1), cleanIdent(ident1, tpe), astParse(on))

    case "take" -@> '{ type t; ($q: Query[`t`]).take($n: Int) } => Take(astParse(q),astParse(n))
    case "drop" -@> '{ type t; ($q: Query[`t`]).drop($n: Int) } => Drop(astParse(q),astParse(n))

    // 2-level so we don't care to select-apply it now
    case "sortBy" -@@> '{ type r; ($q: Query[t]).sortBy[`r`](${Lambda1(ident1, tpe, body)})($ord: Ord[`r`]) } =>
      SortBy(astParse(q), cleanIdent(ident1, tpe), astParse(body), astParse(ord))

    case "groupBy" -@> '{ type r; ($q: Query[t]).groupBy[`r`](${Lambda1(ident1, tpe, body)}) } =>
      GroupBy(astParse(q), cleanIdent(ident1, tpe), astParse(body))

    case "distinct" --> '{ ($q: Query[t]).distinct } =>
      astParse(q) match
        case fj: FlatJoin => throw new IllegalArgumentException(
          """
            |The .distinct cannot be placed after a join clause in a for-comprehension. Put it before.
            |For example. Change:
            |  for { a <- query[A]; b <- query[B].join(...).distinct } to:
            |  for { a <- query[A]; b <- query[B].distinct.join(...) }
            |""".stripMargin
        )
        case other =>
          Distinct(other)

    case "nested" --> '{ ($q: Query[t]).nested } =>
      astParse(q) match
        case fj: FlatJoin => throw new IllegalArgumentException(
          """
            |The .nested cannot be placed after a join clause in a for-comprehension. Put it before.
            |For example. Change:
            |  for { a <- query[A]; b <- query[B].join(...).nested } to:
            |  for { a <- query[A]; b <- query[B].nested.join(...) }
            |""".stripMargin
        )
        case other =>
          io.getquill.ast.Nested(other)

  }

  import io.getquill.JoinQuery

  case class OnClause(ident1: String, tpe1: quotes.reflect.TypeRepr, ident2: String, tpe2: quotes.reflect.TypeRepr, on: quoted.Expr[_])
  object withOnClause:
    def unapply(jq: Expr[_]) =
      jq match
        case '{ ($q: JoinQuery[a,b,r]).on(${Lambda2(ident1, tpe1, ident2, tpe2, on)}) } =>
          Some((UntypeExpr(q), OnClause(ident1, tpe1, ident2, tpe2, on)))
        case _ =>
          None

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

/** Query contains, nonEmpty, etc... Pre-filters for a boolean output type */
case class SetOperationsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.SpecificClause[Boolean, Ast] with PropertyAliases {
  import qctx.reflect.{Constant => TConstant, Ident => TIdent, _}
  import Parser.Implicits._

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ type t; type u >: `t`; ($q: Query[`t`]).nonEmpty } =>
      UnaryOperation(SetOperator.`nonEmpty`, astParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).isEmpty } =>
      UnaryOperation(SetOperator.`isEmpty`, astParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).contains[`u`]($b) } =>
      BinaryOperation(astParse(q), SetOperator.`contains`, astParse(b))
  }

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

/**
 * Since QueryParser only matches things that output Query[_], make a separate parser that
 * parses things like query.sum, query.size etc... when needed.
 */
case class QueryScalarsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with PropertyAliases {
  import qctx.reflect.{Constant => TConstant, Ident => TIdent, _}
  import Parser.Implicits._

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ type t; type u >: `t`; ($q: Query[`t`]).value[`u`] } => astParse(q)
    case '{ type t; type u >: `t`; ($q: Query[`t`]).min[`u`] } => Aggregation(AggregationOperator.`min`, astParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).max[`u`] } => Aggregation(AggregationOperator.`max`, astParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).avg[`u`]($n) } => Aggregation(AggregationOperator.`avg`, astParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).sum[`u`]($n) } => Aggregation(AggregationOperator.`sum`, astParse(q))
    case '{ type t; ($q: Query[`t`]).size } => Aggregation(AggregationOperator.`size`, astParse(q))
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
//     case '{ ($query:Insert[qt]).onConflictIgnore } =>
//       OnConflict(astParse(query), OnConflict.NoTarget, OnConflict.Ignore)
//   }
// }

case class InfixParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with Assignments {
  import quotes.reflect.{Constant => TConstant, _}
  import Parser.Implicits._
  import io.getquill.dsl.InfixDsl

  def delegate: PartialFunction[Expr[_], Ast] =
    case '{ ($i: InfixValue).pure.asCondition } => genericInfix(i)(true, Quat.BooleanExpression)
    case '{ ($i: InfixValue).asCondition } => genericInfix(i)(false, Quat.BooleanExpression)
    case '{ ($i: InfixValue).generic.pure.as[t] } => genericInfix(i)(true, Quat.Generic)
    case '{ ($i: InfixValue).pure.as[t] } => genericInfix(i)(true, InferQuat.of[t])
    case '{ ($i: InfixValue).as[t] } => genericInfix(i)(false, InferQuat.of[t])
    case '{ ($i: InfixValue) } => genericInfix(i)(false, Quat.Value)

  def genericInfix(i: Expr[_])(isPure: Boolean, quat: Quat) =
    val (parts, paramsExprs) = InfixComponents.unapply(i).getOrElse { Parser.throwExpressionError(i, classOf[Infix]) }
    Infix(parts.toList, paramsExprs.map(astParse(_)).toList, isPure, quat)


  object StringContextExpr:
    def staticOrFail(expr: Expr[String]) =
      expr match
        case Expr(str: String) => str
        case _ => Parser.throwExpressionError(expr, "All String-parts of a 'infix' statement must be static strings")
    def unapply(expr: Expr[_]) =
      expr match
        case '{ StringContext.apply(${Varargs(parts)}: _*) } =>
          Some(parts.map(staticOrFail(_)))
        case _ => None

  object InfixComponents:
    def unapply(expr: Expr[_]): Option[(Seq[String], Seq[Expr[Any]])] = expr match
      case '{ InfixInterpolator($partsExpr).infix(${Varargs(params)}: _*) } =>
        val parts = StringContextExpr.unapply(partsExpr).getOrElse { Parser.throwExpressionError(partsExpr, "Cannot parse a valid StringContext") }
        Some((parts, params))
      case _ =>
        None

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
}

case class OperationsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with ComparisonTechniques {
  import quotes.reflect._
  import io.getquill.ast.Infix

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  //Handles named operations, ie Argument Operation Argument
  object NamedOp1 {
    def unapply(expr: Expr[_]): Option[(Expr[_], String, Expr[_])] =
      UntypeExpr(expr) match {
        case Unseal(Apply(Select(Untype(left), op: String), Untype(right) :: Nil)) =>
          Some(left.asExpr, op, right.asExpr)
        case _ =>
          None
      }
  }

  def delegate: PartialFunction[Expr[_], Ast] = {
    case '{ ($str:String).like($other) } =>
      Infix(List(""," like ",""), List(astParse(str), astParse(other)), true, Quat.Value)

    case expr @ NamedOp1(left, "==", right) =>
      equalityWithInnerTypechecksIdiomatic(left.asTerm, right.asTerm)(Equal)
    case expr @ NamedOp1(left, "equals", right) =>
      equalityWithInnerTypechecksIdiomatic(left.asTerm, right.asTerm)(Equal)
    case expr @ NamedOp1(left, "!=", right) =>
      equalityWithInnerTypechecksIdiomatic(left.asTerm, right.asTerm)(NotEqual)

    case NamedOp1(left, "||", right) =>
      BinaryOperation(astParse(left), BooleanOperator.||, astParse(right))
    case NamedOp1(left, "&&", right) =>
      BinaryOperation(astParse(left), BooleanOperator.&&, astParse(right))

    case '{ !($b: Boolean) } => UnaryOperation(BooleanOperator.!, astParse(b))

    // Unary minus symbol i.e. val num: Int = 4; quote { -lift(num) }.
    // This is done term-level or we would have to do it for every numeric type
    case Unseal(Select(num, "unary_-")) if isNumeric(num.tpe) => UnaryOperation(NumericOperator.-, astParse(num.asExpr))

    // In the future a casting system should be implemented to handle these cases.
    // Until then, let the SQL dialect take care of the auto-conversion.
    // This is done term-level or we would have to do it for every numeric type
    // toString is automatically converted into the Apply form i.e. foo.toString automatically becomes foo.toString()
    // so we need to parse it as an Apply. The others don't take arg parens so they are not in apply-form.
    case Unseal(Apply(Select(num, "toString"), List())) if isPrimitive(num.tpe) => astParse(num.asExpr)
    case Unseal(Select(num, "toInt")) if isPrimitive(num.tpe) => astParse(num.asExpr)
    case Unseal(Select(num, "toLong")) if isPrimitive(num.tpe) => astParse(num.asExpr)
    case Unseal(Select(num, "toFloat")) if isPrimitive(num.tpe) => astParse(num.asExpr)
    case Unseal(Select(num, "toDouble")) if isPrimitive(num.tpe) => astParse(num.asExpr)
    case Unseal(Select(num, "toLong")) if isPrimitive(num.tpe) => astParse(num.asExpr)
    case Unseal(Select(num, "toByte")) if isPrimitive(num.tpe) => astParse(num.asExpr)
    case Unseal(Select(num, "toChar")) if isPrimitive(num.tpe) => astParse(num.asExpr)

    // TODO not sure how I want to do this on an SQL level. Maybe implement using SQL function containers since
    // they should be more dialect-portable then just infix
    case '{ ($str: String).length } => Infix(List("Len(",")"), List(astParse(str)), true, Quat.Value)

    //String Operations Cases
    case NamedOp1(left, "+", right) if is[String](left) || is[String](right) =>
      BinaryOperation(astParse(left), StringOperator.+, astParse(right))

    case '{ ($i: String).toString } => astParse(i)
    case '{ ($str:String).toUpperCase } => UnaryOperation(StringOperator.toUpperCase, astParse(str))
    case '{ ($str:String).toLowerCase } => UnaryOperation(StringOperator.toLowerCase, astParse(str))
    case '{ ($str:String).toLong } => UnaryOperation(StringOperator.toLong, astParse(str))
    case '{ ($str:String).toInt } => UnaryOperation(StringOperator.toInt, astParse(str))
    case '{ ($left:String).startsWith($right) } => BinaryOperation(astParse(left), StringOperator.startsWith, astParse(right))
    case '{ ($left:String).split($right:String) } => BinaryOperation(astParse(left), StringOperator.split, astParse(right))

    /*
    //SET Operations
    case '{ ($set:String).contains() } =>
      Conso le.printl("Wow you actually did it!")
    */

    // 1 + 1
    // Apply(Select(Lit(1), +), Lit(1))
    // Expr[_] => BinaryOperation
    case NumericOperation(binaryOperation) =>
      binaryOperation
  }

  object NumericOperation {
    def unapply(expr: Expr[_]): Option[BinaryOperation] = {
      UntypeExpr(expr) match {
        case NamedOp1(left, NumericOpLabel(binaryOp), right) if (isNumeric(left.asTerm.tpe) && isNumeric(right.asTerm.tpe)) =>
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
        case ">=" => Some(NumericOperator.`>=`)
        case "<=" => Some(NumericOperator.`<=`)
        case _ => None
      }
  }
}

/**
 * Should check that something is a null-constant basically before anything else because
 * null-constant can match anything e.g. a (something: SomeValue) clause. Found this out
 * when tried to do just '{ (infix: InfixValue) } and 'null' matched it
 */
case class ValueParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    // Parse Null values
    case Unseal(Literal(NullConstant())) => NullValue
    // For cases where a series of flatMaps returns nothing etc...
    case Unseal(Literal(UnitConstant())) => Constant((), Quat.Value)
    // Parse Constants
    case expr @ Unseal(ConstantTerm(v)) => Constant(v, InferQuat.ofExpr(expr))
    // Parse Option constructors
    case '{ Some.apply[t]($v) } => OptionSome(astParse(v))
    case '{ Option.apply[t]($v) } => OptionApply(astParse(v))
    case '{ None } => OptionNone(Quat.Null)
    case '{ Option.empty[t] } => OptionNone(InferQuat.of[t])
  }
}

case class ComplexValueParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {
    case ArrowFunction(prop, value) =>
      Tuple(List(astParse(prop), astParse(value)))

    // Parse tuples
    case Unseal(Apply(TypeApply(Select(TupleIdent(), "apply"), types), values)) =>
      Tuple(values.map(v => astParse(v.asExpr)))

    case CaseClassCreation(ccName, fields, args) =>
      if (fields.length != args.length)
        throw new IllegalArgumentException(s"In Case Class ${ccName}, does not have the same number of fields (${fields.length}) as it does arguments ${args.length} (fields: ${fields}, args: ${args.map(_.show)})")
      val argsAst = args.map(astParse(_))
      CaseClass(fields.zip(argsAst))

    case id @ Unseal(i @ TIdent(x)) =>
      cleanIdent(i.symbol.name, InferQuat.ofType(i.tpe))
  }
}

case class GenericExpressionsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] with PropertyParser {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}
  import Parser.Implicits._

  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)

  def delegate: PartialFunction[Expr[_], Ast] = {

    case AnyProperty(property) => property

    // If at the end there's an inner tree that's typed, move inside and try to parse again
    case Unseal(Typed(innerTree, _)) =>
      astParse(innerTree.asExpr)

    case Unseal(Inlined(_, _, v)) =>
      astParse(v.asExpr)
  }
}
