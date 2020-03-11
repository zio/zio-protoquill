package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._
import miniquill.quoter.Quoted
import miniquill.quoter._
import io.getquill._
import io.getquill.ast._
import org.junit.Test
import org.junit.Assert._
import miniquill.quoter.QuotationBin
import miniquill.quoter.QuotationVase
import io.getquill.context.ExecutionType

class QueryTest { //hellooooooo

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
      val result = run(people)
      assertEquals("""querySchema("Person").map(x => (x.name, x.age, x.address.street, x.address.zip))""", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
    {
      import sqlCtx._
      val result = sqlCtx.run(people)
      assertEquals("SELECT x.name, x.age, x.street, x.zip FROM Person x", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
  }

  @Test
  def directInRunFunctionTest():Unit = {
    {
      import ctx._
      val result = run(people.map(p => p.name))
      assertEquals("""querySchema("Person").map(p => p.name)""", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
    {
      import sqlCtx._
      val result = sqlCtx.run(people.map(p => p.name))
      assertEquals("SELECT p.name FROM Person p", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
  }

  // person with address mapping to address
  @Test
  def personToAddressMap(): Unit = { //hello
    {
      import ctx._
      val result = run(addresses)
      assertEquals("""querySchema("Person").map(p => (p.address.street, p.address.zip))""", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
    {
      import sqlCtx._
      val result = sqlCtx.run(addresses)
      assertEquals("SELECT p.street, p.zip FROM Person p", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
  }

  @Test
  def personToAddressMapWithLift(): Unit = { //hello
    {
      import ctx._
      inline def extQuery = quote(addresses.map(a => a.street + lift("-ext"))) 
      val result = run(extQuery)
      assertEquals("""querySchema("Person").map(p => p.address.street + ?)""", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
    {
      import sqlCtx._
      inline def extQuery = quote(addresses.map(a => a.street + lift("-ext"))) 
      val result = run(extQuery)
      assertEquals("SELECT p.street || ? FROM Person p", result.string)
      assertEquals(ExecutionType.Static, result.executionType)
    }
  }

  @Test
  def personToAddressMapRuntime(): Unit = {
    {
      assertTrue(
        peopleRuntime match { 
          case Quoted(Entity("Person", List()), List(), _) => true 
          case _ => false
        } 
      )
      
      assertTrue(
        addressesRuntime match { 
          case Quoted(
            Map(QuotationTag(_), Ident("p"), Property(Ident("p"), "address")), 
            List(),
            List(QuotationVase(Quoted(Entity("Person", List()), List(), _), _))
          ) => true
          case _ => false
        }
      )

      import ctx._
      val result = run(addressesRuntime)
      assertEquals("""querySchema("Person").map(p => (p.address.street, p.address.zip))""", result.string)
      assertEquals(ExecutionType.Dynamic, result.executionType)
    }
    {
      import sqlCtx._
      val result = sqlCtx.run(addressesRuntime)
      assertEquals("SELECT p.street, p.zip FROM Person p", result.string)
      assertEquals(ExecutionType.Dynamic, result.executionType)
    }
  }

}