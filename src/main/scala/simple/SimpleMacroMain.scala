package simple

import simple.SimpleMacro._
import scala.quoted._

case class SuperEmb(one:String, two:String)
case class SuperPerson(name:String, age:String, emb: SuperEmb)

@main def runFooify() =
  println(fooifyType[SuperPerson])

// object SimpleMacroMain {


//   def main(args: Array[String]):Unit = {
//     // val result = 
//     //   printThenRun("""stuff(stuff("foo") + "foo")""" , stuff(stuff("foo") + "foo"))

//     //   //printThenRun("foobar", stuff(stuff("foo") + "foo"))
//     // println("Result: " + result)

//     // println(
//     //   //printThenRun("""other stuff""" , stuff(stuff { val v = "foo"; v } + "foo"))
//     //   printThenRun("""stuff { val v = "foo"; v }""" , stuff { val v = "foo"; v })
//     // )

//     // println( //hello
//     //   //printThenRun("""other stuff""" , stuff(stuff { val v = "foo"; v } + "foo"))
//     //   printThenRun("""stuff { val v = "foo"; v }""" , stuff { List("foo", "bar").map(elem => elem + "blah").mkString })
//     // )

//     // object Blah {
//     //   val foo = "foofoo"

//     //   object Waah {
//     //     val bar = "barbar"
//     //   }
//     // }

//     // println( //hello
//     //   printThenRun("""stuff { val v = "foo"; v }""" , stuff { Blah.foo })
//     // )

//     // println( //hello
//     //   printThenRun("""stuff { val v = "foo"; v }""" , stuff { Blah.Waah.bar })
//     // )

//     // println( //helloo
//     //   printThenRun(""" val v = (str: String) => str + "foo"; v("bar") """ , stuff { val v = (str: String) => str + "foo"; v("bar") })
//     // )

//     // printThenRun(""" (str: String) => str + "foo" """ , stuff { (str: String) => str + "foo" })

//     // val l = List(1,2,3)
//     // printThenRun(""" l.map(p => p + 1) """ , stuff ( l.map(p => p + 1) ) )

//     case class Address(street:String, age:Int)
//     case class Person(name:String, age:Int, address: Address)
//     class Query[T] {
//       def map[R](f: T=>R): Query[R] = new Query[R]()
//     }
//     inline def query[T] = new Query[T]

//     val abc = 44
//     inline def intPlusInt = abc+1

//     printThenRun(
//       """ (i:Int) => i+1 """ , 
//       stuff( query[Person].map(p => p.name) ) 
//     )

//     // printThenRun(oo
//     //   """ query[Person].map((p: Person) => p.address) """ , 
//     //   stuff( query[Person].map((p: Person) => p.address) ) 
//     // )

//     // printThenRun(
//     //   """ (i:Int) => i.toString """ , 
//     //   stuff( {def clo[T] = (i:Int) => i.toString} )
//     // )

//     // printThenRun(
//     //   """ (i:T) => i.toString """ , 
//     //   stuff( {def clo[T] = (i:T) => i.toString} )
//     // )

//     //betaReduceMethod((i:Int) => i + 1)

//     // printThenRun("""oooooooo
//     // class MyBean {
//     //   def printStr: String = "theprint"
//     // }
//     //  """,
//     //  stuff { 
//     //   class MyBean {
//     //     def printStr: String = "theprint"
//     //   }
//     // })

//     // printThenRun(
//     //   """ (p: Person) => p.address """ , 
//     //   stuff( (i: Int) => {
//     //     val v = (i: Int) => i + 2
//     //     v(44) + 1
//     //   } )
//     // )
//   }
// }
