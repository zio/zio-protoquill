package io.getquill.context


import scala.quoted._
import io.getquill.norm.BetaReduction
import io.getquill.util.LoadObject 
import io.getquill.parser.Parser
import io.getquill.parser.Parser.Implicits._
import io.getquill.parser.ParserFactory
import io.getquill.generic.ElaborateStructure
import io.getquill.ast.{ Ident => AIdent, Insert => AInsert, _ }
import io.getquill.Quoted
import io.getquill.parser.Lifter
import io.getquill.parser.Unlifter
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._
import io.getquill.metaprog.Extractors
import io.getquill.InsertMeta

object InsertMetaMacro:
  import io.getquill.parser.ParserFactory

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
        case Function(List(param), prop @ Property(_, _)) => 
          BetaReduction(prop, param -> InsertMacro.VIdent)
        case other => 
          quotes.reflect.report.throwError(s"Could not recognize insert exclusion AST: ${other} as a valid exclusion AST")
      }

    //println(s"**************** Excludes Props: ${excludeAstProps} *************")
    
    // Shove those into a tuple and return that
    val excludeTuple = Tuple(excludeAstProps.toList)
    val uuid = Expr(java.util.UUID.randomUUID().toString)
    '{ InsertMeta(Quoted[T](${Lifter.tuple(excludeTuple)}, Nil, Nil), $uuid) }
  }
  

end InsertMetaMacro