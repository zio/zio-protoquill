package miniquill.quoter

import scala.quoted._
import io.getquill.Query
import io.getquill.EntityQuery
import miniquill.parser.TastyMatchersContext

object QueryMacro {
  def apply[T: Type](using Quotes): Expr[EntityQuery[T]] = {
    import quotes.reflect._
    val tmc = new TastyMatchersContext
    import tmc._
    import scala.quoted.Expr.summon
    import miniquill.quoter.QuotationLotExpr
    import miniquill.quoter.QuotationLotExpr._


    Expr.summon[SchemaMeta[T]] match {
      case Some(meta) =>
        meta.reseal match {
          // If it is uprootable, unquote the meta and pass it on
          case QuotationLotExpr(Uprootable(_, _, _, _, _, _)) =>
            '{ $meta.unquote }

          // If it's pluckabke can also return that because the parser/Expr accumulate in Context will find it.
          // I am not sure this has use cases.
          case QuotationLotExpr(Pluckable(_, _, _)) =>
            '{ $meta.unquote }
              
          // In case it's only pointable, need to synthesize a new UID for the quotation
          case QuotationLotExpr(Pointable(_)) => //hello
            UnquoteMacro('{$meta.entity})

          case _ => report.throwError("Invalid Quotation:\n" + meta.show, meta)
        }

      case None => 
        '{ EntityQuery.apply[T] }
    }
  }
}