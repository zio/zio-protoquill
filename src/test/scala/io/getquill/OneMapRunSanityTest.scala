package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._
import io.getquill._
import io.getquill.ast._
import io.getquill.quoter.Quoted
import org.scalatest._
import io.getquill.quat.Quat
import io.getquill.quat.quatOf

class OneMapRunSanityTest extends Spec {
  case class SanePerson(name: String, age: Int)

  "simple test for inline query and map translated to the mirror idiom" in {
    inline def q = quote {
      query[SanePerson] // helloo
    }
    inline def qq = quote {
      q //.map(p => p.age)
    }
    val quat = quatOf[SanePerson]
    qq.ast mustEqual Map(Entity("SanePerson", List(), quat.probit), Ident("p", quat), Property(Ident("p", quat), "name"))
    val ctx = new MirrorContext(MirrorIdiom, Literal)
    import ctx.{given,*}
    val output = ctx.run(qq).string
     output mustEqual """querySchema("SanePerson").map(p => p.name)"""
  }

}
