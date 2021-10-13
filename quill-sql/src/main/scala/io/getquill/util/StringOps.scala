package io.getquill.util

object StringOps {

  implicit class StringOpsExt(str: String) {
    def collapseSpace: String = str.stripMargin.replaceAll("\\s+", " ").trim
  }
}
