package zio.quill.sanity

import zio.quill._
import scala.language.implicitConversions

import zio.quill.ast._
import zio.quill.Quoted

import org.scalatest._
import zio.quill.quat.Quat
import zio.quill.quat.quatOf
import zio.quill.quote
import zio.quill.query
import zio.quill.context.SplicingBehaviorHint
import zio.quill.context.SplicingBehavior

class SimpleMapRunSanityTest extends Spec {
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
