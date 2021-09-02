package io.getquill.metaprog

import io.getquill.parser.ParserFactory
import io.getquill.util.LoadModule
import io.getquill.util.Format
import scala.quoted._
import scala.util.Success
import scala.util.Failure

object SummonParser:
  def apply()(using Quotes): ParserFactory =
    import quotes.reflect._
    Expr.summon[ParserFactory] match
      case Some(factory) =>
        val factoryTypeRepr = factory.asTerm.tpe.widen
        factoryTypeRepr.asType match
          case '[t] =>
            LoadModule[t] match
              case Success(parser) => parser.asInstanceOf[ParserFactory]
              case Failure(e) =>
                report.throwError(s"Could not summon a parser of type ${Format.TypeOf[t]}. A parser must be a static object created in it's own compilation unit. ${e}")
      case None =>
        report.throwError("Could not summon a parser factory")
