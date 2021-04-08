package io.getquill.util.debug

import io.getquill._
import io.getquill.Dsl._

object UseMac {
  case class Person(name: String, age: Int)
  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  def main(args: Array[String]):Unit = {
    val l = List(Person("Joe", 123))
    // NOTE doing querySchema here directly does not seem to work in InsertMacro, need to further test that
    //inline def q = quote { liftQuery(l).foreach(p => querySchema[Person]("thePerson").insert(p)) }

    inline def q = quote { liftQuery(l).foreach(p => query[Person].insert(p)) }

    //inline def q = liftQuery(l).foreach(p => query[Person].insert(p))
    // import io.getquill.context.LiftMacro
    //inline def content = LiftMacro.liftInjectedProductExternal[Person, Int]
    //PrintMac(content)

    //val list = LiftMacro.liftInjectedProductExternal[Person, Int]
    //println( list.map(elem => (elem._1, elem._2.apply(Person("Joe", 123)))) )

    //PrintMac(q) //hellooooooooooooooooooooooooooo

    // Uncomment to get error
    println( run(q) )
  }
}