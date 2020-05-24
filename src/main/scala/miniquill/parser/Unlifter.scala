package miniquill.parser

import scala.quoted._
import scala.quoted.matching._
import io.getquill.ast.{Ident => Idnt, Query => Qry, _}

object Unlifter {
  type Unlift[T] = PartialFunction[Expr[T], T]
}

// TODO Rewrite this the way Parser is written (i.e. with ability to compose???)
class Unlifter(using qctx:QuoteContext) extends PartialFunction[Expr[Ast], Ast] {
  import Unlifter._

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
    case '{ List.apply[$t](${ExprSeq(props)}: _*) } => 
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

  implicit def unliftProperty: Unlift[Property] = {
    // Unlike in liftProperty, we need both variants here since we are matching the scala AST expressions
    case '{ Property(${ast}, ${name}) } =>
      Property(unliftAst(ast), fixedString(name))
    case '{ Property.Opinionated(${ast}, ${name}, ${renameable}, ${visibility}) } =>
      Property.Opinionated(unliftAst(ast), fixedString(name), unliftRenameable(renameable), unliftVisibility(visibility))
  }

  implicit def unliftPropertyAlias: Unlift[PropertyAlias] = {
    case '{ PropertyAlias($a, $b) } => PropertyAlias(a.unliftify, b.unliftify)
  }

  implicit class UnliftExt[T](expr: Expr[T])(implicit u: Unlift[T]) {
    def unliftify: T = u.apply(expr) // seems like 'unlift' is now an actual expr method
  }

  def unliftBase: Unlift[Ast] = {
    // TODO have a typeclass like Splicer to translate constant to strings
    case '{ Constant(${b}) } =>
      Constant(fixedString(b))
    case '{ Entity(${b}, ${l})  } =>
      Entity(fixedString(b), l.unliftify)
    case '{ Function($params, $body) } => Function(params.unliftify, unliftAst(body))
    case '{ FunctionApply($function, $values) } => FunctionApply(function.unliftify, values.unliftify)
    case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(unliftAst(query), unliftAst(alias).asInstanceOf[Idnt], unliftAst(body))
    case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(unliftAst(a), unliftOperator(operator).asInstanceOf[BinaryOperator], unliftAst(b))
    case '{ Property(${ast}, ${name}) } =>
      Property(unliftAst(ast), fixedString(name))
    case '{ScalarTag(${uid})} => 
      ScalarTag(fixedString(uid))
    case '{ QuotationTag($uid) } =>
      QuotationTag(fixedString(uid))
  }

  implicit def unliftOperator: Unlift[Operator] = {
    case '{ NumericOperator.* } =>  NumericOperator.*
    case '{ StringOperator.+ } =>  StringOperator.+
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
    }
    unliftActual
  }
}