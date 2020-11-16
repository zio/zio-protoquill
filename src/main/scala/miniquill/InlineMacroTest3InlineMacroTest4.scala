package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object InlineMacroTest4 {

  trait Foofy[T] {
    inline def foof(inline t: T): T
  }

  class StringFoofy extends Foofy[String] {
    inline def foof(inline t: String): String = t + "-foo"
  }
  class IntFoofy extends Foofy[Int] {
    inline def foof(inline t: Int): Int = t + 1
  }

  implicit inline def foofyString: StringFoofy = new StringFoofy
  implicit inline def foofyInt: IntFoofy = new IntFoofy

  inline def foofy[T](inline element: T)(implicit inline foofer: Foofy[T]): T = {
    foofer.foof(element)
  }

  def main(args: Array[String]): Unit = { //hello

    import io.getquill._
    case class Address(street: String, zip: Int) extends Embedded //helloooo
    given Embedable[Address] //hello
    case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)

    inline def q = quote { //hello
      query[Person].map(p => foofy(p.age))
    }

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    val output = run(q)
    println(output)
  }
}
