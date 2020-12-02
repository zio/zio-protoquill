package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object TypeclassExample_Show {

  import io.getquill._
  case class Person(id: Int, name: String, age: Int)
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  trait Show[T]:
    inline def show(inline t: T): String

  class ShowString extends Show[String]:
    inline def show(inline t: String): String = t + "-suffix"

  class ShowInt extends Show[Int]:
    inline def show(inline t: Int): String = t.toString + "-suffix"

  inline given showString as ShowString = new ShowString
  inline given showInt as ShowInt = new ShowInt

  inline def show[T](inline element: T)(using inline shower: Show[T]): String = {
    shower.show(element)
  }
  inline def q = quote {
    query[Person].map(p => show(p.name) + show(p.age))
  }

  println( run(q) )

  def main(args: Array[String]): Unit = {
  }
}
