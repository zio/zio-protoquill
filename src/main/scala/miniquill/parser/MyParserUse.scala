// package miniquill.parser

// import io.getquill._

// object MyParserUse {
//   import io.getquill.context.ExecutionType

//   def main(args: Array[String]):Unit = {
//     inline def hello = Mac.passThrough("hello")
//     PrintMac(hello)
//     println(hello)
//   }
// }

//     // Something like this should be possible in quill, looks like a proxy-val is generated
//     // could the regular val-parser do that?
//     // val i: Any = ("foo", "bar")
//     // inline def fun = i match {
//     //   case ((a,b), c) => "blah"
//     // }
//     val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//     PrintMac( ctx.executeQuery("foo", null, null, ExecutionType.Static) )

// object InlineVariables3 {
//   def main(args: Array[String]):Unit = {
//     class Space {
//       class InnerSpace {
//         inline def hello = Mac.passThrough("hello")
//       }
//       inline def helloWorld = Mac.passThrough(new InnerSpace().hello + " world")
//     }
//     inline def helloWorldToday = Mac.passThrough(new Space().helloWorld + " today")
//     PrintMac(helloWorldToday)
//     println(helloWorldToday)
//   }
// }
