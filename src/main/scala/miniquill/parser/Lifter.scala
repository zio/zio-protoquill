package miniquill.parser

import scala.quoted._
import scala.quoted.matching.Const
import io.getquill.ast.{Ident => Idnt, Query => Qry, _}

object Lifter {
  type Lift[T] = PartialFunction[T, Expr[T]]
}

// TODO Rewrite this the way Parser is written (i.e. with ability to compose???)
class Lifter(given qctx:QuoteContext) extends PartialFunction[Ast, Expr[Ast]] {
  import qctx.tasty.{Constant => TConstant, _, given _}
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

  implicit def liftList[T](implicit liftElement: Lift[T], t: scala.quoted.Type[T]): Lift[List[T]] = {
    case list: List[T] => Expr.ofList(list.map(liftElement(_)))
  }

  implicit def liftPropertyAlias: Lift[PropertyAlias] = {
    case PropertyAlias(a, b) => '{ PropertyAlias(${a.liftable}, ${b.liftable}) }
  }

  implicit def liftAst: Lift[Ast] = {
    val liftBaseActual = liftBase
    val liftPropertyActual = liftProperty
    def liftActual: Lift[Ast] = {
      case liftPropertyActual(ast) => ast
      case liftBaseActual(ast) => ast
    }
    liftActual
  }

  // TODO Can implement liftConstant now

  def liftBase: Lift[Ast] = {
    // TODO Need some type info to be able to lift a const
    // TODO cover primitive cases? Have something that splices certain things to a string?
    case Constant(v: String) => '{ Constant(${Expr(v)}) }
    case Constant(v: Double) => '{ Constant(${Expr(v)}) }
    case Entity(name: String, list) => '{ Entity(${name.liftable}, ${list.liftable})  }
    case Idnt(name: String) => '{ Idnt(${Expr(name)})  }
    case Map(query: Ast, alias: Idnt, body: Ast) => '{ Map(${liftAst(query)}, ${liftAst(alias).asInstanceOf[Expr[Idnt]]}, ${liftAst(body)})  }
    case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${liftAst(a)}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${liftAst(b)})  }
    case ScalarTag(uid: String) => '{ScalarTag(${Expr(uid)})}
    case QuotationTag(uid: String) => '{QuotationTag(${Expr(uid)})}
  }

  implicit def liftOperator: PartialFunction[Operator, Expr[Operator]] = {
    case NumericOperator.* => '{ NumericOperator.* }
    case StringOperator.+ => '{ StringOperator.+ }
  }
}
