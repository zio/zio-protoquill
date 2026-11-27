package zio.quill.context

import scala.quoted._
import zio.quill.Query
import zio.quill.EntityQuery
import zio.quill.metaprog.Extractors._
import zio.quill.SchemaMeta
import zio.quill.metaprog.QuotationLotExpr
import zio.quill.metaprog.QuotedExpr.UprootableWithLifts
import zio.quill.metaprog.QuotedExpr

object QueryMacro {
  def apply[T: Type](using Quotes): Expr[EntityQuery[T]] = {
    import quotes.reflect._
    import scala.quoted.Expr.summon
    import zio.quill.metaprog.QuotationLotExpr
    import zio.quill.metaprog.QuotationLotExpr._

    // TODO Make QuotationLotExpr(meta.reseal) match Uprootable(_, _, _) for better performance
    Expr.summon[SchemaMeta[T]] match {
      case Some(meta) =>
        meta.reseal match {
          // If it is uprootable, unquote the meta and pass it on
          case QuotationLotExpr(Uprootable(_, _, _)) =>
            '{ $meta.unquote }

          // If it's pluckabke can also return that because the parser/Expr accumulate in Context will find it.
          // I am not sure this has use cases.
          case QuotationLotExpr(Pluckable(_, _, _)) =>
            '{ $meta.unquote }

          // In case it's only pointable, need to synthesize a new UID for the quotation
          case QuotationLotExpr(Pointable(_)) =>
            UnquoteMacro('{ $meta.entity })

          case _ => report.throwError("Invalid Quotation:\n" + meta.show, meta)
        }

      case None =>
        '{ EntityQuery.apply[T] }
    }
  }
} // end QueryMacro
