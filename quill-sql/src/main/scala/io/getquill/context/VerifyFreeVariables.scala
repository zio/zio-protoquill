package io.getquill.context

import io.getquill.ast.Ast
import io.getquill.quotation.FreeVariables

import scala.quoted._
import io.getquill.util.Format

object VerifyFreeVariables:
  def verify(ast: Ast) =
    FreeVariables(ast) match {
      case free if free.isEmpty => Right(ast)
      case free =>
        val firstVar = free.headOption.map(_.name).getOrElse("someVar")
        Left(
          s"""
             |Found the following variables: ${free
              .map(_.name)
              .toList} that seem to originate outside of a `quote {...}` or `run {...}`
             |block. In the AST:
             |${Format(ast.toString)}
             |Quotes and run blocks cannot use values outside their scope directly (with the exception of inline expressions in Scala 3).
             |In order to use runtime values in a quotation, you need to lift them, so instead
             |of this `$firstVar` do this: `lift($firstVar)`.
             |Here is a more complete example:
             |Instead of this: `def byName(n: String) = quote(query[Person].filter(_.name == n))`
             |        Do this: `def byName(n: String) = quote(query[Person].filter(_.name == lift(n)))`
        """.stripMargin
        )
    }
  def apply(ast: Ast)(using Quotes) =
    import quotes.reflect._
    verify(ast) match
      case Right(ast)  => ast
      case Left(error) => report.throwError(error)

  def runtime(ast: Ast) =
    verify(ast) match
      case Right(ast)  => ast
      case Left(error) => throw new IllegalStateException(error)
