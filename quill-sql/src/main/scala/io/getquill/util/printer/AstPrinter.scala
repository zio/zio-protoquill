package io.getquill.util.printer

import pprint.{PPrinter, Tree, Util}

import fansi.Str
import pprint.{ Renderer, Tree, Truncated }
import scala.quoted._
import io.getquill.util.FromMessages

class AstPrinter extends io.getquill.AstPrinter(false, false, FromMessages.traceQuats)
object AstPrinter:
  val astprint = new AstPrinter()
