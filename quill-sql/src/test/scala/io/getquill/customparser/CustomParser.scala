package io.getquill.customparser

import io.getquill.parser.ParserLibrary
import scala.quoted._
import io.getquill.parser.OperationsParser
import io.getquill.ast.Ast
import io.getquill.ast.Infix
import io.getquill.quat.Quat
import io.getquill.parser.engine.ParserChain
import io.getquill.parser.engine.Parser

object CustomOps {
  extension (i: Int)
    def **(exponent: Int) = Math.pow(i, exponent)
}

object CustomParser extends ParserLibrary:
  override def operationsParser(using Quotes) =
    ParserChain.attempt(OperationsParser(_)) orElse
      ParserChain.attempt(CustomOperationsParser(_))

class CustomOperationsParser(rootParse: Parser)(using Quotes) extends Parser(rootParse) {
  import quotes.reflect._
  import CustomOps._
  def attempt =
    case '{ ($i: Int)**($j: Int) } =>
      Infix(
        List("power(", " ,", ")"),
        List(rootParse(i), rootParse(j)), true, Quat.Value)
}
