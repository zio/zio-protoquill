package io.getquill.util.prep

object Module1 {
  //def modDefAp() = "modMethodApValue"
  def modDef = "modMethodValue"
  val modVal = "modMethodValue"

  object Foo {
    //def fooDefAp() = "modMethodApValue"
    def fooDef = "modMethodValue"
    val fooVal = "modMethodValue"

    object Bar {
      //def barMethodAp() = "modMethodApValue"
      def barDev = "modMethodValue"
      val barVal = "modMethodValue"
    }
  }
}

object Main {
    def main(args: Array[String]): Unit = {
    //println( ReflectivePathChainLookup.apply(Module, List("Foo", "Bar", "barVal")) )

    import Module1.Foo
    // .map(_.getName)
    //println(Module.getClass.getFields.toList(0).get(Module))

    println(Module1.getClass)
    println(Module1.getClass.getDeclaredClasses.toList)
    //println(Hierarachies.getClass.getFields.toList(0).get("Hierarachies"))
    println(Module1.getClass.getFields.toList(0).getType.getFields.toList)

    // println(Foo.getClass)
    // println(Foo.getClass.getDeclaredClasses.toList)
    // println(Foo.getClass.getField("MODULE$").get(Foo).getClass.getDeclaredClasses.toList)
    //println(Module.getClass.getDeclaredClasses.toList)



  }
}