package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import io.getquill.dsl.EncodingDsl
import io.getquill.quoter.Quoted
import io.getquill.quoter.QueryMeta
import io.getquill.derived._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.dsl.GenericDecoder
import io.getquill.quoter.Planter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.quoter.QuotationLot
import io.getquill.quoter.QuotedExpr
import io.getquill.quoter.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.quoter.EagerPlanter
import io.getquill.quoter.LazyPlanter
import io.getquill.dsl.GenericEncoder

import io.getquill._

object LiftMacro {
  import scala.quoted._ // Expr.summon is actually from here

  def apply[T, PrepareRow](vvv: Expr[T])(using Quotes, Type[T], Type[PrepareRow]): Expr[T] = {
    import quotes.reflect._
    val uuid = java.util.UUID.randomUUID().toString
    val encoder = 
      Expr.summon[GenericEncoder[T, PrepareRow]] match {
        case Some(enc) => enc
        case None => report.throwError(s"Cannot Find a ${TypeRepr.of[T]} Encoder of ${Printer.TreeShortCode.show(vvv.asTerm)}", vvv)
      }
    '{ EagerPlanter($vvv, $encoder, ${Expr(uuid)}).unquote } //[T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
  }

  def applyLazy[T, PrepareRow](vvv: Expr[T])(using Quotes, Type[T], Type[PrepareRow]): Expr[T] = {
    import quotes.reflect._
    val uuid = java.util.UUID.randomUUID().toString
    '{ LazyPlanter($vvv, ${Expr(uuid)}).unquote } //[T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
  }
}
