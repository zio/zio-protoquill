package io.getquill.util

import io.getquill.AstPrinter
import scala.collection.mutable.{Map => MutableMap}

/**
 * Should eventually unify this with io.getquill.util.Messages in Scala2-Quill
 */
object ProtoMessages:
  private def variable(propName: String, envName: String, default: String) =
    Option(System.getProperty(propName)).orElse(sys.env.get(envName)).getOrElse(default)

  private[getquill] def resetCache(): Unit = cacheMap.clear()
  private val cacheMap: MutableMap[String, Any] = MutableMap()
  private def cache[T](name: String, value: => T): T =
    cacheMap.getOrElseUpdate(name, value).asInstanceOf[T]

  private[getquill] def useStdOut = cache("quill.macro.stdout", variable("quill.macro.stdout", "quill_macro_stdout", "false").toBoolean)
  private[getquill] def serializeAst = cache("quill.ast.serialize", variable("quill.ast.serialize", "quill_ast_serialize", "true").toBoolean)
  private[getquill] def maxQuatFields = cache("quill.quat.tooManyFields", variable("quill.quat.tooManyFields", "quill_quat_tooManyFields", "4").toInt)
  private[getquill] def errorDetail = cache("quill.error.detail", variable("quill.error.detail", "quill_error_detail", "false").toBoolean)

end ProtoMessages
