package io.getquill.context


import scala.quoted._
import io.getquill.norm.BetaReduction
import io.getquill.util.LoadObject 
import io.getquill.parser.Parser
import io.getquill.parser.Parser.Implicits._
import io.getquill.parser.ParserFactory
import io.getquill.generic.ElaborateStructure
import io.getquill.ast.{ Ident => AIdent, Insert => AInsert, Update => AUpdate, _ }
import io.getquill.Quoted
import io.getquill.parser.Lifter
import io.getquill.parser.Unlifter
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._
import io.getquill.metaprog.Extractors
import io.getquill.InsertMeta
import io.getquill.UpdateMeta
import io.getquill.Insert
import io.getquill.Update
import io.getquill.parser.ParserFactory


object MetaMacro:
  def apply[T: Type, P <: ParserFactory: Type](excludesRaw: Expr[Seq[(T => Any)]])(using Quotes): (Tuple, Expr[String]) =
    val parserFactory = LoadObject[P].get

    // Pull out individual args from the apply
    val excludes = excludesRaw match
      case Varargs(exprs) => exprs
      case _ => quotes.reflect.report.throwError(s"Could not parse: ${excludesRaw.show} as a varargs parameter")
    
    // Parse those into Function(params, Property) asts
    val excludeAstMethods =
      excludes.map(exclude => parserFactory.apply.seal.apply(exclude))

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
  end apply
end MetaMacro

object InsertMetaMacro:
  def apply[T: Type, P <: ParserFactory: Type](excludesRaw: Expr[Seq[(T => Any)]])(using Quotes): Expr[InsertMeta[T]] =
    val (excludeTuple, uuid) = MetaMacro[T, P](excludesRaw)
    '{ InsertMeta(Quoted[T](${Lifter.tuple(excludeTuple)}, Nil, Nil), $uuid) }

object UpdateMetaMacro:
  def apply[T: Type, P <: ParserFactory: Type](excludesRaw: Expr[Seq[(T => Any)]])(using Quotes): Expr[UpdateMeta[T]] =
    val (excludeTuple, uuid) = MetaMacro[T, P](excludesRaw)
    '{ UpdateMeta(Quoted[T](${Lifter.tuple(excludeTuple)}, Nil, Nil), $uuid) }
