package zio.quill.util

object StringOps {

  implicit class StringOpsExt(str: String) {
    def collapseSpace: String = str.stripMargin.replaceAll("\\s+", " ").trim
  }
}
