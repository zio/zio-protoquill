package io.getquill.util

object Test {
  def main(args: Array[String]): Unit = {
    inline def f = Foo("vvv")
    Mac.run(f)
  }
}