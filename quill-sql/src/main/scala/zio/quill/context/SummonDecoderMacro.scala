package zio.quill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import zio.quill.generic.Dsl
//import zio.quill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import zio.quill.{ReturnAction}
import zio.quill.generic.EncodingDsl
import zio.quill.Quoted
import zio.quill.QueryMeta
import zio.quill.generic._
import zio.quill.context.mirror.MirrorDecoders
import zio.quill.context.mirror.Row
import zio.quill.generic.GenericDecoder
import zio.quill.Planter
import zio.quill.ast.Ast
import zio.quill.ast.ScalarTag
import scala.quoted._
import zio.quill.idiom.Idiom
import zio.quill.ast.{Transform, QuotationTag}
import zio.quill.QuotationLot
import zio.quill.metaprog.QuotedExpr
import zio.quill.metaprog.PlanterExpr
import zio.quill.idiom.ReifyStatement
import zio.quill.generic.DecodingType

object SummonDecoderMacro {
  import zio.quill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import zio.quill.Planter

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
