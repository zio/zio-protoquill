package miniquill.parser

import io.getquill._

object MyParserUse {
  import io.getquill.context.ExecutionType

  def main(args: Array[String]):Unit = {
    //inline def tup = ("foo", "bar")
    //PrintMac(tup)

    // Something like this should be possible in quill, looks like a proxy-val is generated
    // could the regular val-parser do that?
    // val i: Any = ("foo", "bar")
    // inline def fun = i match {
    //   case ((a,b), c) => "blah"
    // }
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    PrintMac( ctx.executeQuery("foo", null, null, ExecutionType.Static) )

    // val list = List(1,2,3)
    // inline def fun = list.filter { case 1 => true } //hellooooooo
    // PrintMac(fun)
  }
}
