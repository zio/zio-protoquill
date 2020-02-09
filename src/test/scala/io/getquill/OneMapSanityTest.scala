package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._
import io.getquill._
import io.getquill.ast._
import org.junit.Test
import org.junit.Assert._
import miniquill.quoter.Quoted

class OneMapSanityTest {
  case class SanePerson(name: String, age: Int)

  @Test
  def simpleTest() = {
    inline def q = quote {
      query[SanePerson] // hello
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    assertEquals(Map(Entity("SanePerson", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
  }

}
