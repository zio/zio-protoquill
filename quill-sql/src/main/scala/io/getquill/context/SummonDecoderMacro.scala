package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.generic.Dsl
//import io.getquill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ReturnAction}
import io.getquill.generic.EncodingDsl
import io.getquill.Quoted
import io.getquill.QueryMeta
import io.getquill.generic._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericDecoder
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import scala.quoted._
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.generic.DecodingType

object SummonDecoderMacro {
  import io.getquill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import io.getquill.Planter

  def apply[T: Type, ResultRow: Type, Session: Type](using Quotes): Expr[GenericDecoder[ResultRow, Session, T, DecodingType]] = {
    import quotes.reflect._
    Expr.summon[GenericDecoder[ResultRow, Session, T, DecodingType.Specific]] match {
      case Some(decoder) => decoder
      case None =>
        Expr.summon[GenericDecoder[ResultRow, Session, T, DecodingType.Generic]] match {
          case Some(decoder) => decoder
          case None          => report.throwError(s"Cannot Find decoder for ${Type.show[T]}")
        }
    }
  }
}
