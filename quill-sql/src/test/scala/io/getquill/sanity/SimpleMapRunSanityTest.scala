package io.getquill.sanity

import io.getquill._
import scala.language.implicitConversions

import io.getquill.ast._
import io.getquill.Quoted

import org.scalatest._
import io.getquill.quat.Quat
import io.getquill.quat.quatOf
import io.getquill.quote
import io.getquill.query
import io.getquill.context.SplicingBehaviorHint
import io.getquill.context.SplicingBehavior

class SimpleMapRunSanityTest extends MirrorSpec {
  case class SanePerson(name: String, age: Int)

  given SplicingBehaviorHint with {
    override type BehaviorType = SplicingBehavior.FailOnDynamic
  }

  "simple test for inline query and map translated to the mirror idiom" in {
    inline def q = quote {
      query[SanePerson] // helloo
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    val quat = quatOf[SanePerson]
    qq.ast mustEqual Map(Entity("SanePerson", List(), quat.probit), Ident("p", quat), Property(Ident("p", quat), "name"))
    val ctx = new MirrorContext(MirrorIdiom, Literal)
    import ctx._
    val output = ctx.run(qq).string
    output mustEqual """querySchema("SanePerson").map(p => p.name)"""
  }

}
