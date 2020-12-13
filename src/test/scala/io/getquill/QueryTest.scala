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

  // TODO Need to test 3-level injection etc...
  case class Address(street:String, zip:Int) extends Embedded
  case class Person(name: String, age: Int, address: Address)
  val sqlCtx = new MirrorContext(MirrorSqlDialect, Literal)
  val ctx = new MirrorContext(MirrorIdiom, Literal)

  inline def people = quote {
    query[Person]
  }
  inline def addresses = quote {
    people.map(p => p.address)
  }
  def peopleRuntime = quote {
    query[Person]
  }
  // TODO Need join semantics to test these (also add id, fk to case classes)
  // def addressesRuntimePeopleCompiletime = quote {
  //  peopleRuntime.join(address).on(_.name == _.street)
  // }
  // def addressesCompiletimePeopleRuntime = quote {
  //  peopleRuntime.join(addressRuntime).on(_.name == _.street)
  // }
  def addressesRuntime = quote {
    peopleRuntime.map(p => p.address)
  }


  // one level object query
  // @Test
  def oneLevelQuery(): Unit = {
    
  }

  "shuold work correctly with lift" in {
    import ctx._
    val q = quote { lift("hello") }
    println( ctx.run(  peopleRuntime.map(p => p.name + lift("hello"))) )
  }

}