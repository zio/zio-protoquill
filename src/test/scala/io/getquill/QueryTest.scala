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

class QueryTest {

  case class Person(name: String, age: Int)
  val ctx = new MirrorContext(MirrorIdiom, Literal)

  def peopleRuntime = myquote {
    query[Person]
  }
  inline def q2 = myquote {
    peopleRuntime.map(p => p.name)
  }

  def main(args: Array[String]): Unit = {
    import ctx._
    println( ctx.runAndTest( q2 ) ) //hello
  }
}