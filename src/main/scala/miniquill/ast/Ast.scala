package miniquill.ast

sealed trait Ast
sealed trait QueryTree extends Ast
case class Idnt(name: String) extends Ast
case class Map(query: Ast, alias: Idnt, body: Ast) extends QueryTree
case class Entity(name: String) extends QueryTree
case class Property(ast: Ast, name: String) extends Ast

sealed trait Operation extends Ast
case class BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) extends Operation

sealed trait Value extends Ast
case class Const(v: Double) extends Value

/* Operators */
sealed trait Operator
sealed trait BinaryOperator extends Operator
object NumericOperator {
  case object `*` extends BinaryOperator
}

sealed trait Lift extends Ast {
  val uid: String
}

// Don't think we need this intermediate case class
sealed trait ScalarLift extends Lift

// TODO Change to ScalarValueTag
case class ScalarValueLift(uid: String) extends ScalarLift

case class QuotationTag(uid: String) extends Lift
