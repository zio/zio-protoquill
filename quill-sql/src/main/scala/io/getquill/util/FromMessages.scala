package io.getquill.util

import io.getquill.util.Messages

/** Pull out some properties from io.getquill.Messages that are needed in various places but not accessible from there.
  */
object FromMessages {
  def traceQuats = Messages.traceQuats
}
