package zio.quill.sanity

import scala.language.implicitConversions
import zio.quill._

import zio.quill.ast._
import zio.quill.Quoted
import zio.quill.quat.quatOf
import zio.quill.context.SplicingBehaviorHint
import zio.quill.context.SplicingBehavior

class SimpleMapSanityTest extends Spec {
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
