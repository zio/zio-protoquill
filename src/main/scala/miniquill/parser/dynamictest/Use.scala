package miniquill.parser.dynamictest

import miniquill.parser.PrintMac

object Use {
  def main(args: Array[String]): Unit = {
    val p = Person("Joe")
    inline def lifting = Test.macroSplice(p) //helloooioooooooooo
    PrintMac(lifting)

    inline def unlifting = Test.macroPull(lifting)
    println( Test.macroUse(unlifting) )
  }
}
