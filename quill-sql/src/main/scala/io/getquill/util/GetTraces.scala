package io.getquill.util

// Proxy because Messages.traces is package-specific
// TODO Need to change ownership there to getQuill
object GetTraces {
  def apply() = io.getquill.util.Messages.traces
}
