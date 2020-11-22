package miniquill.parser

object MyParserUse {
  def main(args: Array[String]):Unit = {
    //inline def tup = ("foo", "bar")
    //PrintMac(tup)

    // Something like this should be possible in quill, looks like a proxy-val is generated
    // could the regular val-parser do that?
    inline def fun = (a: Int, b: String) => a.toString + b
    inline def pass(tup: (Int, String) => String): String = tup(1, "blah")

    println(pass(fun))

    PrintMac(pass(fun))
  }
}
