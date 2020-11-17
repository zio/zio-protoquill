// package miniquill

// import simple.SimpleMacro._
// import scala.language.implicitConversions
// import miniquill.quoter.Dsl._

// object InlineMacroTest3_InlineLambda {

//   inline def foofy = (str: String) => str + "-foo";

//   def main(args: Array[String]): Unit = {

//     import io.getquill._
//     case class Address(street: String, zip: Int) extends Embedded //helloooo
//     given Embedable[Address] //hello
//     case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)

//     inline def q = quote { //hello
//       query[Person].map(p => foofy(p.name))
//     }

//     val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//     import ctx._

//     val output = run(q)
//     println(output)
//   }
// }
