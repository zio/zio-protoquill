package miniquill.parser

import scala.quoted._
import scala.quoted.Const
import io.getquill.ast.{Ident => Idnt, Query => Qry, _}

object Lifter {
  type Lift[T] = PartialFunction[T, Expr[T]]

  def apply(qctx: QuoteContext): PartialFunction[Ast, Expr[Ast]] =
    new Lifter(using qctx)
}

// TODO Rewrite this the way Parser is written (i.e. with ability to compose???)
class Lifter(using qctx:QuoteContext) extends PartialFunction[Ast, Expr[Ast]] {
  import qctx.tasty.{Constant => TConstant, _}
  import Lifter._

  def apply(ast: Ast): Expr[Ast] = liftAst(ast)
  def isDefinedAt(ast: Ast): Boolean = liftAst.isDefinedAt(ast)

  implicit class LiftExt[T](t: T)(implicit l:Lift[T], tpe: scala.quoted.Type[T]) {
    def liftable: Expr[T] = l.apply(t)
  }

  implicit def liftString: Lift[String] = { case v:String => Expr(v) }
  implicit def liftDouble: Lift[Double] = { case v:Double => Expr(v) }
  implicit def liftFloat: Lift[Float] = { case v:Float => Expr(v) }
  implicit def liftLong: Lift[Long] = { case v:Long => Expr(v) }
  implicit def liftInt: Lift[Int] = { case v:Int => Expr(v) }
  implicit def liftShort: Lift[Short] = { case v:Short => Expr(v) }
  implicit def liftByte: Lift[Byte] = { case v:Byte => Expr(v) }
  implicit def liftChar: Lift[Char] = { case v:Char => Expr(v) }
  implicit def liftBoolean: Lift[Boolean] = { case v:Boolean => Expr(v) }

  implicit def liftRenameable: Lift[Renameable] = {
    case Renameable.ByStrategy => '{ Renameable.ByStrategy }
    case Renameable.Fixed => '{ Renameable.Fixed }
  }

  implicit def liftVisbility: Lift[Visibility] = {
    case Visibility.Visible => '{ Visibility.Visible }
    case Visibility.Hidden => '{ Visibility.Hidden }
  }

  implicit def liftProperty: Lift[Property] = {
    // Don't need the other case since Property.Opinionated will match the object
    case Property.Opinionated(ast: Ast, name: String, renameable: Renameable, visibility: Visibility) => 
      '{ Property.Opinionated(${liftAst(ast)}, ${Expr(name)}, ${liftRenameable(renameable)}, ${liftVisbility(visibility)}) }
  }

  implicit def liftIdent: Lift[Idnt] = {
    case Idnt(name: String) => '{ Idnt(${Expr(name)})  }
  }

  implicit def liftPropertyAlias: Lift[PropertyAlias] = {
    case PropertyAlias(a, b) => '{ PropertyAlias(${a.liftable}, ${b.liftable}) }
  }

  implicit def liftList[T](implicit liftElement: Lift[T], t: scala.quoted.Type[T]): Lift[List[T]] = {
    case list: List[T] => Expr.ofList(list.map(liftElement(_)))
  }

  implicit def liftAst: Lift[Ast] = {
    val liftBaseActual = liftBase
    val liftPropertyActual = liftProperty
    val liftIdentActual = liftIdent
    def liftActual: Lift[Ast] = {
      case liftPropertyActual(ast) => ast
      case liftIdentActual(ast) => ast
      case liftBaseActual(ast) => ast
    }
    liftActual
  }

  // TODO Make liftBase implicit so can do .liftable to any element?
  // TODO Can implement liftConstant now
  def liftBase: Lift[Ast] = {
    // TODO Need some type info to be able to lift a const
    // TODO cover primitive cases? Have something that splices certain things to a string?
    case Constant(v: String) => '{ Constant(${Expr(v)}) }
    case Constant(v: Double) => '{ Constant(${Expr(v)}) }
    case Constant(v: Boolean) => '{ Constant(${Expr(v)}) }
    case Function(params: List[Idnt], body: Ast) => '{ Function(${params.liftable}, ${liftAst(body)}) }
    case FunctionApply(function: Ast, values: List[Ast]) => '{ FunctionApply(${function.liftable}, ${values.liftable}) }
    case Entity(name: String, list) => '{ Entity(${name.liftable}, ${list.liftable})  }
    case Map(query: Ast, alias: Idnt, body: Ast) => '{ Map(${liftAst(query)}, ${liftAst(alias).asInstanceOf[Expr[Idnt]]}, ${liftAst(body)})  }
    case Filter(query: Ast, alias: Idnt, body: Ast) => '{ Filter(${liftAst(query)}, ${liftAst(alias).asInstanceOf[Expr[Idnt]]}, ${liftAst(body)})  }
    case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${liftAst(a)}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${liftAst(b)})  }
    case ScalarTag(uid: String) => '{ScalarTag(${Expr(uid)})}
    case QuotationTag(uid: String) => '{QuotationTag(${Expr(uid)})}
    case Union(a, b) => '{ Union(${liftAst(a)}, ${liftAst(b)}) }
  }

  import EqualityOperator.{ == => ee }

  implicit def liftOperator: PartialFunction[Operator, Expr[Operator]] = {
    case NumericOperator.* => '{ NumericOperator.* }
    case StringOperator.+ => '{ StringOperator.+ }
    case _: ee.type => '{ EqualityOperator.== } // if you don't do it this way, complains about 'stable identifier error'
    case BooleanOperator.|| => '{ BooleanOperator.|| }
  }
}
