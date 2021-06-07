package io.getquill.customparser

import io.getquill.parser.ParserLibrary
import scala.quoted._
import io.getquill.parser.Parser
import io.getquill.parser.OperationsParser
import io.getquill.ast.Ast
import io.getquill.ast.Infix
import io.getquill.quat.Quat

object CustomOps {
  extension (i: Int) {
    def **(other: Int) = i * other * other
  }
}

object CustomParser extends ParserLibrary:
  import Parser._
  override def operationsParser(using qctx: Quotes) =
    Series.of(new OperationsParser, new CustomOperationsParser)

case class CustomOperationsParser(root: Parser[Ast] = Parser.empty)(override implicit val qctx: Quotes) extends Parser.Clause[Ast] {
  import quotes.reflect._
  import CustomOps._
  def reparent(newRoot: Parser[Ast]) = this.copy(root = newRoot)
  def delegate: PartialFunction[Expr[_], Ast] =
    case '{ ($i: Int)**($j: Int) } =>
      Infix(
        List("ttFunc(", " ,", ")"),
        List(astParse(i), astParse(j)), true, Quat.Value)
}
