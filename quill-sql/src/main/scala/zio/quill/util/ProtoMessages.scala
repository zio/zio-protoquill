package zio.quill.util

import zio.quill.AstPrinter
import scala.collection.mutable.{Map => MutableMap}

/**
 * Should eventualy unify this with zio.quill.util.Messages in Scala2-Quill
 */
object ProtoMessages {
  private def variable(propName: String, envName: String, default: String) =
    Option(System.getProperty(propName)).orElse(sys.env.get(envName)).getOrElse(default)

  private[quill] def resetCache(): Unit = cacheMap.clear()
  private val cacheMap: MutableMap[String, Any] = MutableMap()
  private def cache[T](name: String, value: => T): T =
    cacheMap.getOrElseUpdate(name, value).asInstanceOf[T]

  private[quill] def useStdOut = cache("quill.macro.stdout", variable("quill.macro.stdout", "quill_macro_stdout", "false").toBoolean)
  private[quill] def serializeAst = cache("quill.ast.serialize", variable("quill.ast.serialize", "quill_ast_serialize", "true").toBoolean)
  private[quill] def maxQuatFields = cache("quill.quat.tooManyFields", variable("quill.quat.tooManyFields", "quill_quat_tooManyFields", "4").toInt)
  private[quill] def errorDetail = cache("quill.error.detail", variable("quill.error.detail", "quill_error_detail", "false").toBoolean)
  private[quill] def aggressiveQuatChecking = cache("quill.quat.aggresive", variable("quill.quat.aggresive", "quill_quat_aggresive", "false").toBoolean)

} // end ProtoMessages
