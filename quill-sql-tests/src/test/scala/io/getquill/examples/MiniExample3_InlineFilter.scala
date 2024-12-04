package io.getquill.examples

import scala.language.implicitConversions
import io.getquill._
import MirrorContext.Codec.*
import MiniExampleEntities.*

object MiniExample3_InlineFilter {

  def main(args: Array[String]): Unit = {

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    inline def onlyJoes =
      (p: Person) => p.name == "Joe"

    inline def q = quote {
      query[Person].filter(onlyJoes)
    }

    println( run(q) )

    println( List(Person("Joe", 22), Person("Jack", 33)).filter(onlyJoes) )

  }
}
