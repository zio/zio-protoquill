package io.getquill.record

import io.getquill.ast._
import io.getquill.ast.Visibility.Visible
import io.getquill.quat.Quat

/**
 * Lightweight expression wrapper that records column access and builds AST nodes
 * at construction time rather than via macro reflection.
 *
 * These bridge into the existing `io.getquill.ast._` types that `quill-engine`
 * understands, producing the same `Property`, `BinaryOperation`, etc. nodes as
 * the macro-based parser.
 */
case class FieldExpr(entityAlias: String, fieldName: String) {

  def toAst: Ast = {
    Property.Opinionated(Ident(entityAlias, Quat.Generic), fieldName, Renameable.neutral, Visible)
  }

  // Comparison operators — produce BinaryOperation AST nodes
  def ===(other: Any): FilterExpr = {
    FilterExpr(BinaryOperation(toAst, EqualityOperator.`_==`, valueToAst(other)))
  }

  def =!=(other: Any): FilterExpr = {
    FilterExpr(BinaryOperation(toAst, EqualityOperator.`_!=`, valueToAst(other)))
  }

  def >(other: Any): FilterExpr = {
    FilterExpr(BinaryOperation(toAst, NumericOperator.`>`, valueToAst(other)))
  }

  def <(other: Any): FilterExpr = {
    FilterExpr(BinaryOperation(toAst, NumericOperator.`<`, valueToAst(other)))
  }

  def >=(other: Any): FilterExpr = {
    FilterExpr(BinaryOperation(toAst, NumericOperator.`>=`, valueToAst(other)))
  }

  def <=(other: Any): FilterExpr = {
    FilterExpr(BinaryOperation(toAst, NumericOperator.`<=`, valueToAst(other)))
  }

  private def valueToAst(value: Any): Ast = value match {
    case f: FieldExpr   => f.toAst
    case f: FilterExpr  => f.ast
    case s: String      => Constant(s, Quat.Value)
    case i: Int         => Constant(i, Quat.Value)
    case l: Long        => Constant(l, Quat.Value)
    case d: Double      => Constant(d, Quat.Value)
    case b: Boolean     => Constant(b, Quat.Value)
    case bd: BigDecimal => Constant(bd, Quat.Value)
    case null           => NullValue
    case other          => Constant(other, Quat.Generic)
  }
}

/**
 * Represents a boolean filter expression that can be combined with `&&` and `||`.
 */
case class FilterExpr(ast: Ast) {
  def &&(other: FilterExpr): FilterExpr = {
    FilterExpr(BinaryOperation(ast, BooleanOperator.`&&`, other.ast))
  }

  def ||(other: FilterExpr): FilterExpr = {
    FilterExpr(BinaryOperation(ast, BooleanOperator.`||`, other.ast))
  }

  def unary_! : FilterExpr = {
    FilterExpr(UnaryOperation(BooleanOperator.`!`, ast))
  }
}
