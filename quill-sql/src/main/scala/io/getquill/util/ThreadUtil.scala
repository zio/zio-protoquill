package io.getquill.util

object ThreadUtil {
  def currentThreadTrace =
    Thread.currentThread.getStackTrace.map("  " + _.toString).mkString("\n")
}
