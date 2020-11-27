package miniquill.parser

object MyParserUse {
  def main(args: Array[String]):Unit = {
    //inline def tup = ("foo", "bar")
    //PrintMac(tup)

    // Something like this should be possible in quill, looks like a proxy-val is generated
    // could the regular val-parser do that?
    inline def fun = (a: Int, b: String) => a.toString + b
    inline def pass(tup: (Int, String) => String): String = tup(1, "blah")

    class Space {
      class InnerSpace {
        def hello = Mac.passThrough("hello")
      }
      def world = Mac.passThrough(new InnerSpace().hello + " world")
    }
    def today = Mac.passThrough(new Space().world + " today")
    PrintMac(today)
    println(today)

    //Mac.enter(today)

    // inline def suffix = " world"
    // inline def suffix2 = " today!"
    // inline def combo = greeting + suffix + suffix2
    // Mac.enter(combo)

  }
}
