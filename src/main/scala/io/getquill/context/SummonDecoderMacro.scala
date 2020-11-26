package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.dsl.Dsl
//import io.getquill.util.Messages.fail
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
import scala.quoted._
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotedExpr
import miniquill.quoter.ScalarPlanterExpr
import io.getquill.idiom.ReifyStatement

import io.getquill._

object SummonDecoderMacro {
  import miniquill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import miniquill.quoter.ScalarPlanter

  def apply[T: Type, ResultRow: Type](using Quotes): Expr[GenericDecoder[ResultRow, T]] = {
    import quotes.reflect._
    Expr.summon[GenericDecoder[ResultRow, T]] match {
      case Some(decoder) => decoder
      case None => report.throwError(s"Cannot Find decoder for ${Type.show[T]}")
    }
  }  
}
