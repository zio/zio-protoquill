package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.Quoted
import miniquill.quoter._
import io.getquill._
import io.getquill.ast._
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._

class QueryTest extends Spec with Inside { //hellooooooo

  case class Person(name: String, age: Int)
  val ctx = new MirrorContext(MirrorIdiom, Literal)

  def peopleRuntime = quote {
    query[Person]
  }

  "shuold work correctly with lift" in {
    import ctx._
    println( ctx.runAndTest(  peopleRuntime.map(p => p.name)) ) //helloooooooooooooooooooooooooooo
  }
}