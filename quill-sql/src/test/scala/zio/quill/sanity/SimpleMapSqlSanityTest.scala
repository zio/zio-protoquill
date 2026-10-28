package zio.quill.sanity

import scala.language.implicitConversions
import zio.quill._

import zio.quill.ast._
import zio.quill.Quoted

import zio.quill.quat.quatOf
import zio.quill.context.SplicingBehaviorHint
import zio.quill.context.SplicingBehavior

class SimpleMapSqlSanityTest extends Spec {
  case class SanePerson(name: String, age: Int)

  given SplicingBehaviorHint with {
    override type BehaviorType = SplicingBehavior.FailOnDynamic
  }

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
