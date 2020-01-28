package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._
import io.getquill._
import io.getquill.ast._
import org.junit.Test
import org.junit.Assert._
import miniquill.quoter.Quoted

class QuotationTest {
  case class Address(street:String, zip:Int) extends Embedded
  case class Person(name: String, age: Int, address: Address)

  // test a quotation producing an ast
  @Test
  def compiletime_quotationProducingAst() = {
    inline def q = quote {
      query[Person].map(p => p.name) // also try _.name
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), q.ast)
  }

  @Test
  def compiletime_quotationProducingAstUnquote() = {
    inline def q = quote {
      query[Person] // also try _.name
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
  }

  // test a quotation producing an ast
  // note this is actually fine and should be able to produce a query since
  // we are not passing one query into another. I.e. there is no QuotationTag
  // that needs to be joined later
  @Test
  def runtime_quotationProducingAst() = {
    val q = quote {
      query[Person].map(p => p.name) // also try _.name
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), q.ast)
  }

  // test a quotation going into another
  // (with/without auto unquoting?)
  @Test
  def runtime_quotationProducingAstAutoUnquote() = {
    val q = quote {
      query[Person]
    }
    val qq = quote {
      q.map(p => p.name)
    }
    printer.lnf(qq)
    val matches = 
      qq match {
        case Quoted(Map(QuotationTag(_), Ident("p"), Property(Ident("p"), "name")), ()) => true
        case _ => false
      }
    assertTrue(matches)
  }

}




// test a runtime quotation
// test two runtime quotations
// test a runtime going into a compiletime


// test a quotation with a context
// test a quotation producing a variable