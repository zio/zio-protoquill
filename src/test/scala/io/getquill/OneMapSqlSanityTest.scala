package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._
import io.getquill._
import io.getquill.ast._
import io.getquill.quoter.Quoted
import io.getquill._
import io.getquill.quat.quatOf

class OneMapSqlSanityTest extends Spec {
  case class SanePerson(name: String, age: Int)

  "simple test for one inline query converted to sql" in {
    inline def q = quote {
      query[SanePerson] // helloo
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    val quat = quatOf[SanePerson]
    qq.ast mustEqual Map(Entity("SanePerson", List(), quat.probit), Ident("p", quat), Property(Ident("p", quat), "name"))
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    val output = ctx.run(qq).string
     output mustEqual """SELECT p.name FROM SanePerson p"""
  }

}
