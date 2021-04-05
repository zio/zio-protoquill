package io.getquill.util.debug

import io.getquill._
import io.getquill.Dsl._

object UseMac {
  case class Person(name: String, age: Int)
  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  def main(args: Array[String]):Unit = {
    val l = List(Person("Joe", 123))
    inline def q = quote { liftQuery(l).foreach(p => query[Person].insert(p)) }
    //inline def q = liftQuery(l).foreach(p => query[Person].insert(p))
    PrintMac(q)
  }
}