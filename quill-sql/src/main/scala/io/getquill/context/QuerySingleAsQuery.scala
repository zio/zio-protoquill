package io.getquill.context

import scala.quoted._
import io.getquill.Query
import io.getquill.EntityQuery
import io.getquill.metaprog.Extractors._
import io.getquill.SchemaMeta
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotedExpr.UprootableWithLifts
import io.getquill.metaprog.QuotedExpr

object QuerySingleAsQuery:
  import io.getquill._
  import scala.quoted.Expr.summon
  import io.getquill.metaprog.QuotationLotExpr
  import io.getquill.metaprog.QuotationLotExpr._

  inline def apply[T](inline quoted: Quoted[T]): Quoted[Query[T]] = ${ applyImpl[T]('quoted) }
  def applyImpl[T: Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[Quoted[Query[T]]] =
    import quotes.reflect._
    quoted match
      case UprootableWithLifts(QuotedExpr(ast, _, _), lifts) =>
        '{ Quoted[Query[T]]($ast, ${Expr.ofList(lifts.map(_.plant))}, Nil) }
      case _ =>
        '{ Quoted[Query[T]]($quoted.ast, $quoted.lifts, $quoted.runtimeQuotes) }

end QuerySingleAsQuery