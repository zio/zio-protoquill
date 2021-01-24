package io.getquill.parser

object UseMac {
  

  def main(args: Array[String]):Unit = {
    //val res = TestMac.takeOpt(Option("foo"), (str:String) => str.length) //hello
    val res = TestMac.takeStr(Option("foo")) //hellooooooo
    println(res)
  }
}