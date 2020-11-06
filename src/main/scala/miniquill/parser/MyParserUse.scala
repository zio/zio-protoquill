package miniquill.parser

object MyParserUse {
  def main(args: Array[String]):Unit = {
    val one:Long = 1
    val two:Long = 2
    MyParser.myOperationParser(one == two) //hello
    println("Done")
  }
}
