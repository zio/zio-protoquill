package miniquill.parser

object MyParserUse {
  def main(args: Array[String]):Unit = {
    //inline def tup = ("foo", "bar")
    //PrintMac(tup)

    // Something like this should be possible in quill, looks like a proxy-val is generated
    // could the regular val-parser do that?
    // val i: Any = ("foo", "bar")
    // inline def fun = i match {
    //   case ((a,b), c) => "blah"
    // }
    MatchMac({val v = "hello"; val vv = "hello"; v + vv})

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
