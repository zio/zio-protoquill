package zio.quill.util

// Proxy because Messages.traces is package-specific
// TODO Need to change ownership there to getQuill
object GetTraces {
  def apply() = zio.quill.util.Messages.traces
}
