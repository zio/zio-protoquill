package io.getquill

object Format {
  // import org.scalafmt.interfaces.Scalafmt
  // import org.scalafmt.cli.Scalafmt210

  def apply(code: String) = {
      val formatted:String = code
        // new Scalafmt210().format(
        //   s"""|object Foo {
        //       |  ${code}
        //       |}""".stripMargin, 
        //   "Main.scala")
        formatted
      
        //.replaceFirst("object Foo {","")
        //.replaceAll("}$","")
    }
}