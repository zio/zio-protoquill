package io.getquill.context

import io.getquill.context.MyContext
import io.getquill.context.MirrorType._

object ContextUse {
  def main(args: Array[String]): Unit = {
    val ctx = new MyContext();
    import ctx._
    val tup = ("foo", 1)
    println( tup.mirrorType )
  }
}

