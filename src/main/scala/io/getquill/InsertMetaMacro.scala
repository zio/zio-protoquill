package io.getquill

import io.getquill._
import scala.quoted._
import miniquill.quoter.UnquoteMacro
import io.getquill.norm.BetaReduction
import io.getquill.util.LoadObject 
import miniquill.parser.Parser
import miniquill.parser.Parser.Implicits._
import miniquill.parser.ParserFactory
import io.getquill.derived.Expander
import io.getquill.ast.{ Ident => AIdent, Insert => AInsert, _ }
import miniquill.quoter.ExtractLifts
import miniquill.quoter.Quoted
import miniquill.parser.Lifter
import miniquill.parser.Unlifter
import miniquill.quoter.UnquoteMacro
import miniquill.quoter.QueryMacro
import miniquill.quoter.QuotationLotExpr
import miniquill.quoter.QuotationLotExpr._
import miniquill.parser.TastyMatchers
import miniquill.quoter.InsertMeta

object InsertMetaMacro:
  import miniquill.parser.ParserFactory

  def apply[T: Type, P <: ParserFactory: Type](excludesRaw: Expr[Seq[(T => Any)]])(using Quotes): Expr[InsertMeta[T]] = {
    val parserFactory = LoadObject[P].get

    //println(s"**************** RAW Excludes: ${excludesRaw} *************")

    // Pull out individual args from the apply
    val excludes = excludesRaw match
      case Varargs(exprs) => exprs
      case _ => quotes.reflect.report.throwError(s"Could not parse: ${excludesRaw.show} as a varargs parameter")
    
    // Parse those into Function(params, Property) asts
    val excludeAstMethods =
      excludes.map(exclude => parserFactory.apply.seal.apply(exclude))

    //println(s"**************** Excludes ASTs: ${excludeAstMethods} *************")
    
      // Excract the 'Property' elements from there
    val excludeAstProps = 
      excludeAstMethods.map {
        case Function(List(param), prop @ Property(_, _)) => prop
        case other => quotes.reflect.report.throwError(s"Could not recognize insert exclusion AST: ${other} as a valid exclusion AST")
      }

    //println(s"**************** Excludes Props: ${excludeAstProps} *************")
    
    // Shove those into a tuple and return that
    val excludeTuple = Tuple(excludeAstProps.toList)
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    '{ InsertMeta(Quoted[T](${Lifter.tuple(excludeTuple)}, Nil, Nil), $uuid) }
  }
  

end InsertMetaMacro