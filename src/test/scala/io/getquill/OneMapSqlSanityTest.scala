package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._
import io.getquill._
import io.getquill.ast._
import org.junit.Test
import org.junit.Assert._
import miniquill.quoter.Quoted
import io.getquill._

class OneMapSqlSanityTest {
  case class SanePerson(name: String, age: Int)

  @Test
  def simpleTest() = {
    inline def q = quote {
      query[SanePerson] // helloo
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    assertEquals(Map(Entity("SanePerson", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    val output = run(qq).string
    assertEquals("""SELECT p.name FROM SanePerson p""", output)
  }

}
