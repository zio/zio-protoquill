package io.getquill.util.debug

object UseMac {
  

  def main(args: Array[String]):Unit = {
    inline def l = List("foo", Option(Option("bar")), Option(2), Option(Option(Option("baz"))))
    println( TestMac.flattenOpt(l) )//hellooooooo
  }
}