package io.getquill.util

object StringUtil {

  def section(code: String): String = {
    val lines = code.split("\n")
    lines.tail.foldLeft("  |" + lines.head) { (out, line) =>
      out + '\n' +
        (if (line.isEmpty) line else "  |" + line)
    }
  }

}
