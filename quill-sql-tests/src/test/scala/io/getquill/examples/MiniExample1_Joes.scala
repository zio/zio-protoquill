package io.getquill.examples

import scala.language.implicitConversions
import io.getquill._

object MiniExample1_Joes {

  case class Person(name: String, age: Int)

  def main(args: Array[String]): Unit = {

    inline def joes(inline q: Query[Person], inline filter: Boolean) =
      inline if (filter)
        q.filter(p => p.name == "Joe")
      else
        q

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    inline def q1 = quote {
      joes(query[Person], true)
    }
    inline def q2 = quote {
      joes(query[Person], false)
    }

    println(run(q1))
    println(run(q2))
  }
}
