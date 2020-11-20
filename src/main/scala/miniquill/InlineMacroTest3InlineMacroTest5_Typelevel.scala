package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object InlineMacroTest5_Typelevel {

  import io.getquill._
  case class Address(street: String, zip: Int) extends Embedded //helloooo
  given Embedable[Address] //hello
  case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  trait Conv[In]:
    type Out
    inline def get(inline i: In): Out
  
  class ConvString extends Conv[String]:
    type Out = Int
    inline def get(inline i: String): Int = i.length + 55
  
  class ConvInt extends Conv[Int]:
    type Out = String
    inline def get(inline t: Int): String = t.toString + "suffix1"
  
  inline given convString as ConvString = new ConvString
  inline given convInt as ConvInt = new ConvInt

  inline def conv[T](inline element: T)(using inline conv: Conv[T]): conv.Out =
    conv.get(element)
  
  inline def q1 = quote { query[Person].map(p => (conv(p.name) + 66, conv(p.age) + "suffix2")) }
  println( run(q1) )

  // inline def q1 = quote { query[Person].map(p => conv(p.name) + 66) }
  // println(run(q1))

  // inline def q2 = quote { query[Person].map(p => conv(p.age) + "suffix2") }
  // println(run(q2))





    // helloooo
    
    // Tuple blows up in expr model
    //query[Person].map(p => (p.name, p.age))

    

  def main(args: Array[String]): Unit = { //hellooooooo


  }
}
