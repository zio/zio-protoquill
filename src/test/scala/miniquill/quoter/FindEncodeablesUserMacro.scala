package miniquill.quoter

import miniquill.quoter.ScalarPlanter
import miniquill.quoter.ScalarPlanterExpr
import scala.quoted._

// Need to create a example user macro of FindEncodeables to be able to test it
object FindScalarPlantersUserMacro {
  inline def apply[PrepareRow](inline encodeables: List[ScalarPlanter[Any, Any]], inline prep: PrepareRow): List[(Any, PrepareRow)] = ${ applyImpl[PrepareRow]('encodeables, 'prep) }
  def applyImpl[PrepareRow: Type](encodeables: Expr[List[ScalarPlanter[Any, Any]]], prep: Expr[PrepareRow])(given qctx: QuoteContext): Expr[List[(Any, PrepareRow)]] = {
    import qctx.tasty.{given, _}
    val foundEncodeables: List[ScalarPlanter[Any, Any]] = 
      ScalarPlanerExpr.InlineList(encodeables)

    val encoded =
      foundEncodeables.zipWithIndex.map((enc, idx) => {
        '{ (${enc.value}, ${enc.encoder}.apply(${Expr(idx)}, ${enc.value}, ${prep})) }
      })

    Expr.ofList(encoded)
  }

}