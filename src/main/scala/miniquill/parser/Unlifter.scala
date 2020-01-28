package miniquill.parser

import scala.quoted._
import io.getquill.ast.{Ident => Idnt, Constant => Const, Query => Qry, _}

object Unlifter {
  type Unlift[T] = PartialFunction[Expr[T], T]
}
class Unlifter(given qctx:QuoteContext) extends PartialFunction[Expr[Ast], Ast] {
  import Unlifter._

  def apply(astExpr: Expr[Ast]): Ast = unliftAst(astExpr)
  def isDefinedAt(astExpr: Expr[Ast]): Boolean = unliftAst.isDefinedAt(astExpr)
  private def fixedString(expr: Expr[Any]): String = 
    expr match {
      case scala.quoted.matching.Const(v: String) => v
    }

  def unliftVisibility: Unlift[Visibility] = {
    case '{ Visibility.Hidden } => Visibility.Hidden
    case '{ Visibility.Visible } => Visibility.Visible
  }

  def unliftRenameable: Unlift[Renameable] = {
    case '{ Renameable.ByStrategy } => Renameable.ByStrategy
    case '{ Renameable.Fixed } => Renameable.Fixed
  }

  def unliftProperty: Unlift[Property] = {
    // Unlike in liftProperty, we need both variants here since we are matching the scala AST expressions
    case '{ Property(${ast}, ${name}) } =>
      Property(unliftAst(ast), fixedString(name))
    case '{ Property.Opinionated(${ast}, ${name}, ${renameable}, ${visibility}) } =>
      Property.Opinionated(unliftAst(ast), fixedString(name), unliftRenameable(renameable), unliftVisibility(visibility))
  }

  def unliftBase: Unlift[Ast] = {
    // TODO have a typeclass like Splicer to translate constant to strings
    case '{ Const(${b}) } =>
      Const(fixedString(b))
    case '{ Entity(${b}, ${l})  } =>
      Entity(fixedString(b), List())
    case '{ Idnt(${b}) } =>
      Idnt(fixedString(b))
    case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(unliftAst(query), unliftAst(alias).asInstanceOf[Idnt], unliftAst(body))
    case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(unliftAst(a), unliftOperator(operator).asInstanceOf[BinaryOperator], unliftAst(b))
    case '{ Property(${ast}, ${name}) } =>
      Property(unliftAst(ast), fixedString(name))
    case '{ScalarValueTag(${uid})} => 
      ScalarValueTag(fixedString(uid))
    case '{ QuotationTag($uid) } =>
      QuotationTag(fixedString(uid))
  }

  def unliftOperator: Unlift[Operator] = {
    case '{ NumericOperator.* } =>  NumericOperator.*
  }

  def unliftAst: Unlift[Ast] = {
    // Doing it this way so that users can override the unlift functions
    // in a custom parser
    val unliftBaseActual = unliftBase
    val unliftRenableableActual = unliftRenameable
    val unliftPropertyActual = unliftProperty
    def unliftActual: Unlift[Ast] = {
      case unliftPropertyActual(ast) => ast
      case unliftBaseActual(ast) => ast
    }
    unliftActual
  }
}