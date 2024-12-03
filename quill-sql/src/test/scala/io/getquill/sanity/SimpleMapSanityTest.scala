package io.getquill.sanity

import scala.language.implicitConversions
import io.getquill._

import io.getquill.ast._
import io.getquill.Quoted
import io.getquill.quat.quatOf
import io.getquill.context.SplicingBehaviorHint
import io.getquill.context.SplicingBehavior

class SimpleMapSanityTest extends MirrorSpec {
  case class SanePerson(name: String, age: Int)

  given SplicingBehaviorHint with {
    override type BehaviorType = SplicingBehavior.FailOnDynamic
  }

  "simple test for inline query and map" in {
    inline def q = quote {
      query[SanePerson]
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    val quat = quatOf[SanePerson]
    qq.ast mustEqual Map(Entity("SanePerson", List(), quat.probit), Ident("p", quat), Property(Ident("p", quat), "name"))
  }

}
