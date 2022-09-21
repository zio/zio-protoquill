// package io.getquill.util.debug

// import io.getquill._

// object PrintMacExample {
//   case class Person(name: String, age: Int)

//   // This class needs to be in it's own compilation unit separate from Parsing.scala etc...
//   // While it can remain in the main codebase for convenience reasons, it has to be commented
//   // in case files like Parsing.scala need to be recompiled
//   def main(args: Array[String]): Unit = {
//     val ctx = new SqlMirrorContext(PostgresDialect, Literal)
//     import ctx._
//     inline def q = quote {
//       query[Person]
//     }
//     PrintMac(q)
//   }
// }
