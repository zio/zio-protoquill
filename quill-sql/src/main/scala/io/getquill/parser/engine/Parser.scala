package io.getquill.parser.engine

import scala.quoted.*
import io.getquill.ast.Ast
import io.getquill.context.LRUCache
import io.getquill.parser.ParserHelpers
import io.getquill.util.Format

object ParserNodeCache {
  val cache: LRUCache[(String, Expr[_]), Option[Ast]] = new LRUCache(100000)
  def getOrDefault(key: (String, Expr[_]), default: => Option[Ast]) = cache.getOrDefault(key, default)
}

trait Parser(rootParse: Parser | Parser.Nil)(using Quotes) {
  def name: String
  import quotes.reflect._
  def apply(input: Expr[_])(using History): Ast = attempt.lift(input).getOrElse(error(input))
  protected def error(input: Expr[_]): Nothing = failParse(input, classOf[Ast])
  protected def attempt: History ?=> PartialFunction[Expr[_], Ast]
  // Attempt the parser externally. Usually this is the just the `attempt` method
  // but in some cases we might want early-exist functionality.
  private[engine] def attemptProper: History ?=> Function[Expr[_], Option[Ast]] = { (expr: Expr[_]) =>
    // First fix up all test errors then attempt to use this cache
    attempt.lift(expr)
    //ParserNodeCache.getOrDefault((name, expr), attempt.lift(expr))
  }


}

object Parser {
  sealed trait Nil
  object Nil extends Nil

  def empty(rootParse: Parser | Parser.Nil)(using Quotes) =
    new Parser(rootParse) {
      val name = "empty-parser"
      protected def attempt: History ?=> PartialFunction[Expr[_], Ast] = PartialFunction.empty
    }

  /** Optimizes 'Clause' by checking if it is some given type first. Otherwise can early-exit */
  trait PrefilterType[Criteria: Type](using Quotes) extends Parser {
    import quotes.reflect._
    def prefilter(expr: Expr[_]): Boolean =
      expr.asTerm.tpe <:< TypeRepr.of[Criteria]
  }

  /** Optimizes 'Clause' by allowing a more efficient 'prematch' criteria to be used */
  trait Prefilter(using Quotes) extends Parser {
    def prefilter(expr: Expr[_]): Boolean
    private[engine] override def attemptProper: History ?=> Function[Expr[_], Option[Ast]] =
      (input: Expr[_]) => if (prefilter(input)) attempt.lift(input) else None
  }

} // end Parser
