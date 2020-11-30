package miniquill.parser

object InlineVariables_Flat {
  def main(args: Array[String]): Unit = {
    inline def greeting = "hello"
    inline def suffix = " world"
    inline def suffix2 = " today!"
    inline def combo = greeting + suffix + suffix2
    Mac.enter(combo)
  }
}

object InlineVariables0{
  def main(args: Array[String]):Unit = {
    inline def hello = Mac.passThrough("hello")
    PrintMac(hello)
    println(hello)
  }
}

object InlineVariables2 {
  def main(args: Array[String]):Unit = {
    class Space {
      inline def world = Mac.passThrough("hello")
    }
    inline def helloWorld = Mac.passThrough(new Space().world + " world")
    PrintMac(helloWorld)
    println(helloWorld)
  }
}

object InlineVariables3 {
  def main(args: Array[String]):Unit = {
    class Space {
      class InnerSpace {
        inline def hello = Mac.passThrough("hello")
      }
      inline def helloWorld = Mac.passThrough(new InnerSpace().hello + " world")
    }
    inline def helloWorldToday = Mac.passThrough(new Space().helloWorld + " today")
    PrintMac(helloWorldToday)
    println(helloWorldToday)
  }
}
