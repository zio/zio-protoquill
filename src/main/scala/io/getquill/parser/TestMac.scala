package io.getquill.parser

import scala.quoted._
import io.getquill.parser.TastyMatchers

object TestMac {
  inline def takeOpt[T,R](opt: Option[T], f: T=>R):Option[R] = ${ takeOptImpl('opt, 'f) }
  def takeOptImpl[T:Type,R:Type](opt: Expr[Option[T]], f: Expr[T=>R])(using qctx: Quotes): Expr[Option[R]] =
    '{ $opt.map(prop => $f(prop)) }

  inline def takeStr(opt: Option[String]):Option[Any] = ${ takeStrImpl('opt) }
  def takeStrImpl(opt: Expr[Option[String]])(using qctx: Quotes): Expr[Option[Any]] = {
    import qctx.reflect._
    class Opt(using val qctx: Quotes) extends TastyMatchers {
      def apply: Expr[Option[Any]] = {
        //'{ $opt.map($prop => ${`.`(prop.asInstanceOf[Expr[String]])("length")}.asInstanceOf[Int]) }
        //'{ $opt.map(prop => ${'{prop} `.` ("length") }) }
        '{ $opt.map(prop => ${'prop `.` ("length") }) }
        //Lambda(opt.asTerm.termSymbol, MMethod)
      }
    }
    new Opt().apply
  }
    
}