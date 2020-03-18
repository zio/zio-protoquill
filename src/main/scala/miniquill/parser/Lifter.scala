package miniquill.parser

import scala.quoted._
import io.getquill.ast.{Ident => Idnt, Constant => Const, Query => Qry, _}

object Lifter {
  type Lift[T] = PartialFunction[T, Expr[T]]
}

// TODO Rewrite this the way Parser is written (i.e. with ability to compose???)
class Lifter(given qctx:QuoteContext) extends PartialFunction[Ast, Expr[Ast]] {
  import qctx.tasty.{_, given _}
  import Lifter._

  def apply(ast: Ast): Expr[Ast] = liftAst(ast)
  def isDefinedAt(ast: Ast): Boolean = liftAst.isDefinedAt(ast)

  def liftRenameable: Lift[Renameable] = {
    case Renameable.ByStrategy => '{ Renameable.ByStrategy }
    case Renameable.Fixed => '{ Renameable.Fixed }
  }

  def liftVisbility: Lift[Visibility] = {
    case Visibility.Visible => '{ Visibility.Visible }
    case Visibility.Hidden => '{ Visibility.Hidden }
  }

  def liftProperty: Lift[Property] = {
    // Don't need the other case since Property.Opinionated will match the object
    case Property.Opinionated(ast: Ast, name: String, renameable: Renameable, visibility: Visibility) => 
      '{ Property.Opinionated(${liftAst(ast)}, ${Expr(name)}, ${liftRenameable(renameable)}, ${liftVisbility(visibility)}) }
  }

  def liftAst: Lift[Ast] = {
    val liftBaseActual = liftBase
    val liftPropertyActual = liftProperty
    def liftActual: Lift[Ast] = {
      case liftPropertyActual(ast) => ast
      case liftBaseActual(ast) => ast
    }
    liftActual
  }

  def liftBase: Lift[Ast] = {
    // TODO Need some type info to be able to lift a const
    // TODO cover primitive cases? Have something that splices certain things to a string?
    case Const(v: String) => '{ Const(${Expr(v)}) }
    case Const(v: Double) => '{ Const(${Expr(v)}) }
    case Entity(name: String, list) => '{ Entity(${Expr(name)}, List())  }
    case Idnt(name: String) => '{ Idnt(${Expr(name)})  }
    case Map(query: Ast, alias: Idnt, body: Ast) => '{ Map(${liftAst(query)}, ${liftAst(alias).asInstanceOf[Expr[Idnt]]}, ${liftAst(body)})  }
    case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${liftAst(a)}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${liftAst(b)})  }
    case ScalarTag(uid: String) => '{ScalarTag(${Expr(uid)})}
    case QuotationTag(uid: String) => '{QuotationTag(${Expr(uid)})}
  }

  def liftOperator: PartialFunction[Operator, Expr[Operator]] = {
    case NumericOperator.* => '{ NumericOperator.* }
    case StringOperator.+ => '{ StringOperator.+ }
  }
}
