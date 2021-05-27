package io.getquill.util

import io.getquill.AstPrinter
import fansi.Str
import io.getquill.ast.Renameable.{ ByStrategy, Fixed }
import io.getquill.ast.Visibility.{ Hidden, Visible }
import io.getquill.ast._
import io.getquill.quat.Quat
import io.getquill.util.Messages.QuatTrace
import pprint.{ Renderer, Tree, Truncated }
import scala.quoted._

class BetterAstPrinter(traceOpinions: Boolean, traceAstSimple: Boolean, traceQuats: QuatTrace)(using Quotes) extends AstPrinter(traceOpinions, traceAstSimple, traceQuats) {
  import quotes.reflect.{ Tree => TTree, _ }
  override def additionalHandlers: PartialFunction[Any, Tree] =
    val overrides: PartialFunction[Any, Tree] =
      //case tpe: Quotes#reflectModule#TypeRepr => Tree.Literal(Printer.TypeReprShortCode.show(tpe.asInstanceOf[quotes.reflect.TypeRepr].widen))
      case tpe: Quotes#reflectModule#TypeTree => Tree.Literal(tpe.asInstanceOf[quotes.reflect.TypeTree].show)
    overrides.orElse(super.additionalHandlers)

}

import io.getquill.util.Messages
def qprintMac(using Quotes) = new BetterAstPrinter(Messages.traceOpinions, Messages.traceAstSimple, Messages.traceQuats)