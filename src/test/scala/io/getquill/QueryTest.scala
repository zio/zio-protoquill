package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._
import miniquill.quoter.Quoted
import miniquill.quoter._
import io.getquill._
import io.getquill.ast._
import miniquill.quoter.QuotationBin
import miniquill.quoter.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._

object GreenEggsAndHam {
  inline def hereTheyAre: String = "yay, here they are"
}

class QueryTest extends Spec with Inside { //hellooooooo

  case class Person(name: String, age: Int)
  inline def people = quote {
    query[Person]
  }

  
  inline def blah = quote { // helloooooooooooooooooo
    people.foobar(GreenEggsAndHam.hereTheyAre)
  }
  println(blah.ast)


}