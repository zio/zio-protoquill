package io.getquill.util

//import io.getquill.AstPrinter
//import io.getquill.idiom.Idiom

object Messages {

  private def variable(propName: String, envName: String, default: String) =
    Option(System.getProperty(propName)).orElse(sys.env.get(envName)).getOrElse(default)

  private[util] val prettyPrint = variable("quill.macro.log.pretty", "quill_macro_log", "false").toBoolean
  private[util] val debugEnabled = variable("quill.macro.log", "quill_macro_log", "true").toBoolean
  private[util] val traceEnabled = variable("quill.trace.enabled", "quill_trace_enabled", "false").toBoolean
  private[util] val traceColors = variable("quill.trace.color", "quill_trace_color,", "false").toBoolean
  private[util] val traceOpinions = variable("quill.trace.opinion", "quill_trace_opinion", "false").toBoolean
  private[util] val traceAstSimple = variable("quill.trace.ast.simple", "quill_trace_ast_simple", "false").toBoolean
  private[util] val traces: List[TraceType] =
    variable("quill.trace.types", "quill_trace_types", "standard")
      .split(",")
      .toList
      .map(_.trim)
      .flatMap(trace => TraceType.values.filter(traceType => trace == traceType.value))

  def tracesEnabled(tt: TraceType) =
    traceEnabled && traces.contains(tt)

  sealed trait TraceType { def value: String }
  object TraceType {
    case object Quotation extends TraceType { val value = "quotation" }
    case object Normalizations extends TraceType { val value = "norm" }
    case object Standard extends TraceType { val value = "standard" }
    case object NestedQueryExpansion extends TraceType { val value = "nest" }
    case object AvoidAliasConflict extends TraceType { val value = "alias" }

    def values: List[TraceType] = List(Standard, Normalizations, NestedQueryExpansion, AvoidAliasConflict, Quotation)
  }

  def fail(msg: String) =
    throw new IllegalStateException(msg)

}