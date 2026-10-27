package zio.quill.customparser

import zio.quill.parser.ParserLibrary
import scala.quoted._
import zio.quill.parser.OperationsParser
import zio.quill.ast.Ast
import zio.quill.ast.Infix
import zio.quill.quat.Quat
import zio.quill.parser.engine.ParserChain
import zio.quill.parser.engine.Parser
import zio.quill.norm.TranspileConfig

object CustomOps {
  extension (i: Int) {
    def **(exponent: Int) = Math.pow(i, exponent)
  }
}

object CustomParser extends ParserLibrary {
  override def operationsParser(using Quotes, TranspileConfig) =
    ParserChain.attempt(OperationsParser(_)) orElse
      ParserChain.attempt(CustomOperationsParser(_))
}

class CustomOperationsParser(rootParse: Parser)(using Quotes) extends Parser(rootParse) {
  import quotes.reflect._
  import CustomOps._
  def attempt = {
    case '{ ($i: Int) ** ($j: Int) } =>
      Infix(
        List("power(", " ,", ")"),
        List(rootParse(i), rootParse(j)),
        true,
        false,
        Quat.Value
      )
  }
}
