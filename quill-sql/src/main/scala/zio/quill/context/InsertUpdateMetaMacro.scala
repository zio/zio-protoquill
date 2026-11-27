package zio.quill.context

import scala.quoted._
import zio.quill.norm.BetaReduction
import zio.quill.util.Load
import zio.quill.parser.ParserFactory
import zio.quill.generic.ElaborateStructure
import zio.quill.ast.{Ident => AIdent, Insert => AInsert, Update => AUpdate, _}
import zio.quill.Quoted
import zio.quill.parser.Lifter
import zio.quill.parser.Unlifter
import zio.quill.metaprog.QuotationLotExpr
import zio.quill.metaprog.QuotationLotExpr._
import zio.quill.metaprog.Extractors
import zio.quill.InsertMeta
import zio.quill.UpdateMeta
import zio.quill.Insert
import zio.quill.Update
import zio.quill.parser.ParserFactory
import zio.quill.metaprog.SummonParser
import zio.quill.metaprog.SummonTranspileConfig
import zio.quill.parser.engine.History
import zio.quill.norm.TranspileConfig

object MetaMacro {
  def apply[T: Type](excludesRaw: Expr[Seq[(T => Any)]])(using Quotes): (Tuple, Expr[String]) = {
    val parser = SummonParser().assemble
    given TranspileConfig = SummonTranspileConfig()

    // Pull out individual args from the apply
    val excludes = excludesRaw match {
      case Varargs(exprs) => exprs
      case _              => quotes.reflect.report.throwError(s"Could not parse: ${excludesRaw.show} as a varargs parameter")
    }

    // Parse those into Function(params, Property) asts
    val excludeAstMethods =
      excludes.map(exclude => parser(exclude))

    // Excract the 'Property' elements from there
    val excludeAstProps =
      excludeAstMethods.map {
        case Function(List(param), prop @ Property(_, _)) =>
          BetaReduction(prop, param -> InsertUpdateMacro.VIdent)
        case other =>
          quotes.reflect.report.throwError(s"Could not recognize insert exclusion AST: ${other} as a valid exclusion AST")
      }

    // Shove those into a tuple and return that
    val excludeTuple = Tuple(excludeAstProps.toList)
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    (excludeTuple, uuid)
  } // end apply
} // end MetaMacro

object InsertMetaMacro {
  def apply[T: Type](excludesRaw: Expr[Seq[(T => Any)]])(using Quotes): Expr[InsertMeta[T]] = {
    val (excludeTuple, uuid) = MetaMacro[T](excludesRaw)
    '{ InsertMeta(Quoted[T](${ Lifter.tuple(excludeTuple) }, Nil, Nil), $uuid) }
  }
}

object UpdateMetaMacro {
  def apply[T: Type](excludesRaw: Expr[Seq[(T => Any)]])(using Quotes): Expr[UpdateMeta[T]] = {
    val (excludeTuple, uuid) = MetaMacro[T](excludesRaw)
    '{ UpdateMeta(Quoted[T](${ Lifter.tuple(excludeTuple) }, Nil, Nil), $uuid) }
  }
}
