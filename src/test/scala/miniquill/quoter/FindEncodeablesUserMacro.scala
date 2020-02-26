package miniquill.quoter

import miniquill.quoter.ScalarValueVase
import miniquill.quoter.{SummonInlineEncodeables, InlineEncodeable}
import scala.quoted._

// Need to create a example user macro of FindEncodeables to be able to test it
object FindEncodeablesUserMacro {
  inline def apply[PrepareRow](inline encodeables: List[ScalarValueVase[_]], inline prep: PrepareRow): List[(Any, PrepareRow)] = ${ applyImpl[PrepareRow]('encodeables, 'prep) }
  def applyImpl[PrepareRow: Type](encodeables: Expr[List[ScalarValueVase[_]]], prep: Expr[PrepareRow])(given qctx: QuoteContext): Expr[List[(Any, PrepareRow)]] = {
    import qctx.tasty.{given, _}
    val foundEncodeables: List[InlineEncodeable[PrepareRow]] = 
      SummonInlineEncodeables.apply[PrepareRow](encodeables)

    val encoded =
      foundEncodeables.zipWithIndex.map((enc, idx) => {
        '{ (${enc.value}, ${enc.encoder}.apply(${Expr(idx)}, ${enc.value}, ${prep})) }
      })

    Expr.ofList(encoded)
  }

}