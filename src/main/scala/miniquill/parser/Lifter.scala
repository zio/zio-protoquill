package miniquill.parser

import scala.quoted._
import scala.quoted.Const
import scala.quoted
import io.getquill.ast.{Ident => AIdent, Query => AQuery, _}
import miniquill.parser.TastyMatchers._

object Lifter {

  def apply(ast: Ast): Quotes ?=> Expr[Ast] = liftableAst.apply(ast) // can also do ast.lift but this makes some error messages simpler

  extension [T](t: T)(using Liftable[T], Quotes):
    def expr: Expr[T] = Expr(t)


  import scala.reflect.ClassTag;
  import scala.reflect.classTag;

  trait NiceLiftable[T: ClassTag] extends Liftable[T]:
    def toExpr(t: T): Quotes ?=> Expr[T] = apply(t)
    def lift: Quotes ?=> PartialFunction[T, Expr[T]]
    def apply(t: T): Quotes ?=> Expr[T] = 
      lift.lift(t).getOrElse { throw new IllegalArgumentException(s"Could not Lift AST type ${classTag[T].runtimeClass.getSimpleName} from the element ${pprint.apply(t)} into the Quill Abstract Syntax Tree") }
    def unapply(t: T)(using Quotes) = Some(apply(t))

  given liftRenameable as NiceLiftable[Renameable]:
    def lift =
      case Renameable.ByStrategy => '{ Renameable.ByStrategy }
      case Renameable.Fixed => '{ Renameable.Fixed }

  given liftVisbility as NiceLiftable[Visibility] {
    def lift =
      case Visibility.Visible => '{ Visibility.Visible }
      case Visibility.Hidden => '{ Visibility.Hidden }
  }

  given liftProperty as NiceLiftable[Property] {
    def lift = {
      // Don't need the other case since Property.Opinionated will match the object
      // Note: don't declare variable called 'ast' since it is used internally
      case Property.Opinionated(core: Ast, name: String, renameable: Renameable, visibility: Visibility) => 
        '{ Property.Opinionated(${core.expr}, ${name.expr}, ${renameable.expr}, ${visibility.expr}) }
    }
  }

  given liftIdent as NiceLiftable[AIdent] {
    def lift =
      case AIdent(name: String) => '{ AIdent(${name.expr})  }
  }

  given liftPropertyAlias as NiceLiftable[PropertyAlias] {
    def lift =
      case PropertyAlias(a, b) => '{ PropertyAlias(${a.expr}, ${b.expr}) }
  }

  given liftableAssignment as NiceLiftable[Assignment] {
    def lift =
      case Assignment(ident, property, value) => '{ Assignment(${ident.expr}, ${property.expr}, ${value.expr}) }
  }

  given liftableJoinType as NiceLiftable[JoinType] {
    def lift =
      case InnerJoin => '{ InnerJoin }
      case LeftJoin => '{ LeftJoin }
      case RightJoin => '{ RightJoin }
      case FullJoin => '{ FullJoin }
  }

  given liftOptionOperation as NiceLiftable[OptionOperation] {
    def lift =
      case OptionIsEmpty(a) => '{ OptionIsEmpty(${a.expr}) }
      case OptionMap(a, b, c) => '{ OptionMap(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionTableMap(a, b, c) => '{ OptionTableMap(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionExists(a, b, c) => '{ OptionExists(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionTableExists(a, b, c) => '{ OptionTableExists(${a.expr}, ${b.expr}, ${c.expr}) }
  }

  given liftableAst as NiceLiftable[Ast] {
    def lift =
      case Constant(tmc.ConstantValue(v)) => '{ Constant(${tmc.ConstantExpr(v)}) }
      case Function(params: List[AIdent], body: Ast) => '{ Function(${params.expr}, ${body.expr}) }
      case FunctionApply(function: Ast, values: List[Ast]) => '{ FunctionApply(${function.expr}, ${values.expr}) }
      case Entity(name: String, list) => '{ Entity(${name.expr}, ${list.expr})  }
      case Map(query: Ast, alias: AIdent, body: Ast) => '{ Map(${query.expr}, ${alias.expr}, ${body.expr})  }
      case FlatMap(query: Ast, alias: AIdent, body: Ast) => '{ FlatMap(${query.expr}, ${alias.expr}, ${body.expr})  }
      case Filter(query: Ast, alias: AIdent, body: Ast) => '{ Filter(${query.expr}, ${alias.expr}, ${body.expr})  }
      case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${a.expr}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${b.expr})  }
      case ScalarTag(uid: String) => '{ScalarTag(${uid.expr})}
      case QuotationTag(uid: String) => '{QuotationTag(${uid.expr})}
      case Union(a, b) => '{ Union(${a.expr}, ${b.expr}) }
      case Insert(query: Ast, assignments: List[Assignment]) => '{ Insert(${query.expr}, ${assignments.expr}) }
      case Infix(parts, params, pure) => '{ Infix(${parts.expr}, ${params.expr}, ${pure.expr}) }
      case Tuple(values) => '{ Tuple(${values.expr}) }
      case Join(typ, a, b, identA, identB, body) => '{ Join(${typ.expr}, ${a.expr}, ${b.expr}, ${identA.expr}, ${identB.expr}, ${body.expr}) }
      case FlatJoin(typ, a, identA, on) => '{ FlatJoin(${typ.expr}, ${a.expr}, ${identA.expr}, ${on.expr}) }
      case NullValue => '{ NullValue }
      case v: Property => liftProperty(v)
      case v: AIdent => liftIdent(v)
      case v: OptionOperation => liftOptionOperation(v)
  }

  import EqualityOperator.{ == => ee }

  given liftOperator as NiceLiftable[Operator] {
    def lift =
      case NumericOperator.+ => '{ NumericOperator.+ }
      case NumericOperator.- => '{ NumericOperator.- }
      case NumericOperator.* => '{ NumericOperator.* }
      case NumericOperator./ => '{ NumericOperator./ }
      case NumericOperator.% => '{ NumericOperator.% }
      case NumericOperator.> => '{ NumericOperator.> }
      case NumericOperator.< => '{ NumericOperator.< }
      case StringOperator.+ => '{ StringOperator.+ }
      case _: ee.type => '{ EqualityOperator.== } // if you don't do it this way, complains about 'stable identifier error'
      case BooleanOperator.|| => '{ BooleanOperator.|| }
      case BooleanOperator.&& => '{ BooleanOperator.&& }
  }
}
