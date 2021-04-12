package io.getquill

import io.getquill._
import io.getquill.Dsl._

object QueryTest {
  case class Person(name: String, age: Int)

  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._
  import io.getquill.context.mirror.Row
  import io.getquill.generic.DecodeAlternate

  def main(args: Array[String]): Unit = {
    val resultRow = Row("name" -> "Joe", "age" -> 123)
    
    // This works
    //println( DecodeAlternate.apply[(String, Int), ResultRow](0, resultRow) )
    
    // This does not
    //inline def q = quote { query[Person].map(p => (p.name, p.age)) }
    //val m = run(q)

  }
}