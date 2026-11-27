package zio.quill.parser.engine

import scala.quoted._
import zio.quill.ast.{Ast, NullValue}

object PrefilterProbe {
  inline def apply(inline booleanInput: Boolean, inline matches: Boolean): (Int, Int, Int, Boolean) =
    ${ probe('booleanInput, 'matches) }

  private def probe(booleanInput: Expr[Boolean], matches: Expr[Boolean])(using Quotes): Expr[(Int, Int, Int, Boolean)] = {
    var filters = 0
    var patterns = 0
    var fallbacks = 0
    val shouldMatch = matches.valueOrAbort
    val parser = new Parser(Parser.Nil) with Parser.PrefilterType[Boolean] {
      override def prefilter(expr: Expr[_]): Boolean = {
        filters += 1
        super.prefilter(expr)
      }
      def attempt: History ?=> PartialFunction[Expr[_], Ast] = {
        case _ if { patterns += 1; shouldMatch } => NullValue
      }
    }
    given History = History.Root
    val input: Expr[_] = if (booleanInput.valueOrAbort) Expr(true) else Expr(1)
    val result = parser.attemptProper.orElse[Expr[_], Ast] {
      case _ => fallbacks += 1; NullValue
    }.lift(input)
    Expr((filters, patterns, fallbacks, result.isDefined))
  }
}
