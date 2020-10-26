package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import miniquill.dsl.EncodingDsl
import miniquill.quoter.Quoted
import miniquill.quoter.QueryMeta
import io.getquill.derived._
import miniquill.context.mirror.MirrorDecoders
import miniquill.context.mirror.Row
import miniquill.dsl.GenericDecoder
import miniquill.quoter.ScalarPlanter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotedExpr
import miniquill.quoter.ScalarPlanterExpr
import io.getquill.idiom.ReifyStatement

import io.getquill._

object LiftMacro {
  import scala.quoted._ // Expr.summon is actually from here
  import miniquill.quoter.ScalarPlanter
  import miniquill.dsl.GenericEncoder

  def apply[T, PrepareRow](vvv: Expr[T])(using qctx: QuoteContext, tType: Type[T], prepareRowType: Type[PrepareRow]): Expr[T] = {
    import qctx.tasty._
    val uuid = java.util.UUID.randomUUID().toString
    val encoder = 
      Expr.summon(using '[GenericEncoder[$tType, $prepareRowType]]) match {
        case Some(enc) => enc
        case None => report.throwError(s"Cannot Find encode for ${tType.unseal}", vvv)
      }
    '{ ScalarPlanter($vvv, $encoder, ${Expr(uuid)}).unquote } //[$tType, $prepareRowType] // adding these causes assertion failed: unresolved symbols: value Context_this
  }
}
