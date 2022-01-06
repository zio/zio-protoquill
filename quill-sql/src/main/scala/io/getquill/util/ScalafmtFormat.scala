package io.getquill.util

import org.scalafmt.cli._
import scala.util.matching.Regex
import java.io.File
import metaconfig._, Configured._
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.{ScalafmtRunner => SRunner}
import org.scalafmt.util.LoggerOps._
import io.getquill.util.CommonExtensions.Throwable._

/**
 * Based on ScalaFmt210 from scalafmt cli
 */
object ScalafmtFormat:
  def apply(code: String, showErrorTrace: Boolean = false): String =
    val style = ScalafmtConfig.default
    Scalafmt.format(code, style, Set.empty, "<input>") match
      case Formatted.Success(formattedCode) =>
        formattedCode
      case Formatted.Failure(e) =>
        if (showErrorTrace)
          println(
            s"""===== Failed to format the code ====
              |$code
              |---
              |${e.stackTraceToString}.
              |""".stripMargin
          )
        code
