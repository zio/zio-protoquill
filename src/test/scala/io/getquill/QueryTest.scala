package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._
import io.getquill._
import org.junit.Test
import org.junit.Assert._

class QueryTest {

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
  // should also test this
  //def addressesRuntimePeopleCompiletime = quote {
  //  people.map(p => p.address)
  //}
  def addressesRuntime = quote {
    peopleRuntime.map(p => p.address)
  }


  // one level object query
  @Test
  def oneLevelQuery(): Unit = {
    
  }

  // nested object query (person with address)
  @Test
  def doubleLayerTest():Unit = {
    {
      import ctx._
      assertEquals(
        """querySchema("Person").map(x => (x.name, x.age, x.address.street, x.address.zip))""", 
        run(people).string)
    }
    {
      import sqlCtx._
      assertEquals("SELECT x.name, x.age, x.street, x.zip FROM Person x", sqlCtx.run(people).string)
    }
  }

  // person with address mapping to address
  @Test
  def personToAddressMap(): Unit = {
    {
      import ctx._
      assertEquals("""querySchema("Person").map(p => (p.address.street, p.address.zip))""", run(addresses).string)
    }
    {
      import sqlCtx._
      println(sqlCtx.run(addresses).string)
      assertEquals("SELECT p.street, p.zip FROM Person p", sqlCtx.run(addresses).string)
    }
  }

  @Test
  def personToAddressMapRuntime(): Unit = {
    {
      import ctx._
      println("***************** Expanded ***************")
      printer.lnf(expandAst(addressesRuntime))

      assertEquals("""querySchema("Person").map(p => (p.address.street, p.address.zip))""", run(addressesRuntime).string)
    }
    // {
    //   import sqlCtx._
    //   println(sqlCtx.run(addresses).string)
    //   assertEquals("SELECT p.street, p.zip FROM Person p", sqlCtx.run(addresses).string)
    // }
  }

}