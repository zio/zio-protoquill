package io.getquill

import io.getquill.quoter.Dsl._
import io.getquill.quoter.Dsl.autoQuote
import io.getquill.parser._

object QueryParserTest {
  def main(args: Array[String]):Unit = {
  
    case class Person(name: String, age: Int)

    val a = 2
    val b = 3

    val q = quote { query[Person].take(a + b) }
    //compile

    println(q)
    run(q)
  }
}