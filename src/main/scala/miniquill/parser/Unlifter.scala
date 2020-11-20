package miniquill.parser

import scala.quoted._

import io.getquill.ast.{Ident => Idnt, Query => Qry, _}

object UnlifterType {
  type Unlift[T] = PartialFunction[Expr[T], T]
}

// TODO Rewrite this the way Parser is written (i.e. with ability to compose???)
class Unlifter(using val qctx:QuoteContext) extends PartialFunction[Expr[Ast], Ast] with TastyMatchers {
  import UnlifterType._

  def apply(astExpr: Expr[Ast]): Ast = unliftAst(astExpr)
  def isDefinedAt(astExpr: Expr[Ast]): Boolean = unliftAst.isDefinedAt(astExpr)
  private def fixedString(expr: Expr[Any]): String = 
    expr match { case Const(v: String) => v }

  implicit def unliftString: Unlift[String] = { case Const(v: String) => v }
  implicit def unliftDouble: Unlift[Double] = { case Const(v: Double) => v }
  implicit def unliftFloat: Unlift[Float] = { case Const(v: Float) => v }
  implicit def unliftLong: Unlift[Long] = { case Const(v: Long) => v }
  implicit def unliftInt: Unlift[Int] = { case Const(v: Int) => v }
  implicit def unliftShort: Unlift[Short] = { case Const(v: Short) => v }
  implicit def unliftByte: Unlift[Byte] = { case Const(v: Byte) => v }
  implicit def unliftChar: Unlift[Char] = { case Const(v: Char) => v }
  implicit def unliftBoolean: Unlift[Boolean] = { case Const(v: Boolean) => v }

  implicit def unliftList[T](implicit u: Unlift[T]): Unlift[List[T]] = {
    case '{ Nil } => Nil
    case '{ List.apply[$t](${Varargs(props)}: _*) } => 
      props.toList.map(p => u.apply(p.asInstanceOf[Expr[T]])) // Can we get the type into here so we don't have to cast?
  }

  implicit def unliftVisibility: Unlift[Visibility] = {
    case '{ Visibility.Hidden } => Visibility.Hidden
    case '{ Visibility.Visible } => Visibility.Visible
  }

  implicit def unliftRenameable: Unlift[Renameable] = {
    case '{ Renameable.ByStrategy } => Renameable.ByStrategy
    case '{ Renameable.Fixed } => Renameable.Fixed
  }

  implicit def unliftIdent: Unlift[Idnt] = {
    case '{ Idnt(${Const(name: String)}) } => Idnt(name)
  }

  implicit def unliftJoinType: Unlift[JoinType] = {
    case '{ InnerJoin } => InnerJoin
    case '{ LeftJoin } => LeftJoin
    case '{ RightJoin } => RightJoin
    case '{ FullJoin } => FullJoin
  }

  implicit def unliftProperty: Unlift[Property] = {
    // Unlike in liftProperty, we need both variants here since we are matching the scala AST expressions
    case '{ Property(${ast}, ${name}) } =>
      Property(unliftAst(ast), fixedString(name))
    case '{ Property.Opinionated(${ast}, ${name}, ${renameable}, ${visibility}) } =>
      Property.Opinionated(unliftAst(ast), fixedString(name), unliftRenameable(renameable), unliftVisibility(visibility))
  }

  implicit def unliftAssignment: Unlift[Assignment] = {
    case '{ Assignment($alias, $property, $value) } => Assignment(alias.unliftExpr, property.unliftExpr, value.unliftExpr)
  }

  implicit def unliftPropertyAlias: Unlift[PropertyAlias] = {
    case '{ PropertyAlias($paths, $b) } => PropertyAlias(paths.unliftExpr, b.unliftExpr)
  }

  implicit class UnliftExt[T](expr: Expr[T])(implicit u: Unlift[T]) {
    def unliftExpr: T = u.apply(expr)
  }

  def unliftBase: Unlift[Ast] = {
    // TODO have a typeclass like Splicer to translate constant to strings
    case '{ Constant(${Const(b)}: Double) } => Constant(b)
    case '{ Constant(${Const(b)}: Boolean) } => Constant(b)
    case '{ Constant(${Const(b)}: String) } => Constant(b)
    case '{ Constant(${Const(b)}: Int) } => Constant(b)
    case '{ Entity.apply(${Const(b: String)}, ${elems})  } =>
      Entity(b, elems.unliftExpr)
    case '{ Function($params, $body) } => Function(params.unliftExpr, unliftAst(body))
    case '{ FunctionApply($function, $values) } => FunctionApply(function.unliftExpr, values.unliftExpr)
    case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(unliftAst(query), unliftAst(alias).asInstanceOf[Idnt], unliftAst(body))
    case '{ FlatMap(${query}, ${alias}, ${body}: Ast) } => FlatMap(unliftAst(query), unliftAst(alias).asInstanceOf[Idnt], unliftAst(body))
    case '{ Filter(${query}, ${alias}, ${body}: Ast) } => Filter(unliftAst(query), unliftAst(alias).asInstanceOf[Idnt], unliftAst(body))
    case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(unliftAst(a), unliftOperator(operator).asInstanceOf[BinaryOperator], unliftAst(b))
    case '{ Property(${ast}, ${name}) } =>
      Property(unliftAst(ast), fixedString(name))
    case '{ScalarTag(${uid})} => 
      ScalarTag(fixedString(uid))
    case '{ QuotationTag($uid) } =>
      QuotationTag(fixedString(uid))
    case '{ Union($a, $b) } => Union(unliftAst(a), unliftAst(b))
    case '{ Insert($query, $assignments) } => Insert(query.unliftExpr, assignments.unliftExpr)
    case '{ Infix($parts, $params, $pure) } => Infix(parts.unliftExpr, params.unliftExpr, pure.unliftExpr)
    case '{ Tuple($values) } => Tuple(values.unliftExpr)
    case '{ Join($typ, $a, $b, $aliasA, $aliasB, $on) } => Join(typ.unliftExpr, a.unliftExpr, b.unliftExpr, aliasA.unliftExpr, aliasB.unliftExpr, on.unliftExpr)
    case '{ FlatJoin($typ, $a, $aliasA, $on) } => FlatJoin(typ.unliftExpr, a.unliftExpr, aliasA.unliftExpr, on.unliftExpr)
  }

  implicit def unliftOperator: Unlift[Operator] = {
    case '{ NumericOperator.+ } =>  NumericOperator.+
    case '{ NumericOperator.- } =>  NumericOperator.-
    case '{ NumericOperator.* } =>  NumericOperator.*
    case '{ NumericOperator./ } =>  NumericOperator./
    case '{ NumericOperator.% } =>  NumericOperator.%
    case '{ StringOperator.+ } =>  StringOperator.+
    case '{ EqualityOperator.== } =>  EqualityOperator.==
    case '{ BooleanOperator.|| } =>  BooleanOperator.||
  }

  implicit def unliftAst: Unlift[Ast] = {
    // Doing it this way so that users can override the unlift functions
    // in a custom parser
    val unliftBaseActual = unliftBase
    val unliftPropertyActual = unliftProperty
    val unliftIdentActual = unliftIdent
    val unliftRenableableActual = unliftRenameable
    
    def unliftActual: Unlift[Ast] = {
      case unliftPropertyActual(ast) => ast
      case unliftIdentActual(ast) => ast
      case unliftBaseActual(ast) => ast
      case other =>
        report.throwError("========= Cannot Unlift: =========\n" + io.getquill.Format(other.show))
    }
    unliftActual
  }
}