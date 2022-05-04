package io.getquill.sanity

import scala.language.implicitConversions
import io.getquill._

import io.getquill.ast._
import io.getquill.Quoted

import io.getquill.quat.quatOf
import io.getquill.context.SplicingBehaviorHint
import io.getquill.context.SplicingBehavior

class SimpleMapSqlSanityTest extends Spec {
  case class SanePerson(name: String, age: Int)

  given SplicingBehaviorHint with
    override type BehaviorType = SplicingBehavior.FailOnDynamic

  "simple test for one inline query converted to sql" in {
    inline def q = quote {
      query[SanePerson]
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
