package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object InlineMacroTest4 {

  // trait Foofy[T] {
  //   inline def foof(inline t: T): T
  // }

  // implicit def stringFoofy: Foofy[String] = new Foofy[String]() {
  //   inline def foof(inline t: String): String = t + "-foo"
  // }

  implicit inline def foofyString: String => String = (str: String) => str + "-foofoo";
  implicit inline def foofyInt: Int => Int = (i: Int) => i + 1;

  inline def foofy[T](inline element: T)(implicit inline foofer: T => T): T = {
    foofer(element)
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
