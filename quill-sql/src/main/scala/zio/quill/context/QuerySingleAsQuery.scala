package zio.quill.context

import scala.quoted._
import zio.quill.Query
import zio.quill.EntityQuery
import zio.quill.metaprog.Extractors._
import zio.quill.SchemaMeta
import zio.quill.metaprog.QuotationLotExpr
import zio.quill.metaprog.QuotedExpr.UprootableWithLifts
import zio.quill.metaprog.QuotedExpr

object QuerySingleAsQuery {
  import zio.quill._
  import scala.quoted.Expr.summon
  import zio.quill.metaprog.QuotationLotExpr
  import zio.quill.metaprog.QuotationLotExpr._

  inline def apply[T](inline quoted: Quoted[T]): Quoted[Query[T]] = ${ applyImpl[T]('quoted) }
  def applyImpl[T: Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[Quoted[Query[T]]] = {
    import quotes.reflect._
    quoted match {
      case UprootableWithLifts(QuotedExpr(ast, _, _), lifts) =>
        '{ Quoted[Query[T]]($ast, ${ Expr.ofList(lifts.map(_.plant)) }, Nil) }
      case _ =>
        '{ Quoted[Query[T]]($quoted.ast, $quoted.lifts, $quoted.runtimeQuotes) }
    }
  }

} // end QuerySingleAsQuery
