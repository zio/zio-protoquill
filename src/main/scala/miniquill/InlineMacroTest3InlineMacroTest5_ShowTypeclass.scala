package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object InlineMacroTest5 {

  trait Show[T] {
    inline def show(inline t: T): String
  }

  class ShowString extends Show[String] {
    inline def show(inline t: String): String = t + ""
  }

  // TODO implement parsing for .toString and do that to an int
  class ShowInt extends Show[Int] {
    inline def show(inline t: Int): String = "num"
  }

  inline given showString as ShowString = new ShowString
  inline given showInt as ShowInt = new ShowInt

  // TODO Try returning T, says can't parse the tree not sure why not
  inline def show[T](inline element: T)(implicit inline shower: Show[T]): String = {
    shower.show(element)
  }

  def main(args: Array[String]): Unit = { //hello

    import io.getquill._
    case class Address(street: String, zip: Int) extends Embedded //helloooo
    given Embedable[Address] //hello
    case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)

    inline def q = quote { //hello
      query[Person].map(p => show(p.name) + show(p.age))
    }

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    val output = run(q)
    println(output)
  }
}
