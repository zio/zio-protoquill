package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.Quoted
import miniquill.quoter._
import io.getquill.ast._
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotationVase
import io.getquill.context.ExecutionType

class QueryTestMain {

  case class Person(name: String, age: Int)
  val ctx = new MirrorContext(MirrorIdiom, Literal)

  def peopleRuntime = quote {
    query[Person]
  }
  

  def main(args: Array[String]):Unit = {
    import ctx._
    val result = ctx.run(peopleRuntime.map(p => p.name + lift("hello")))
    println( result.string == """querySchema("Person").map(p => p.name + ?)""" )
    println( result.executionType == ExecutionType.Dynamic )
  }

}