package io.getquill.sanity

//import scala.language.implicitConversions
import io.getquill.*
import io.getquill.ast.*
import io.getquill.Quoted
import io.getquill.quat.quatOf
import io.getquill.context.SplicingBehaviorHint
import io.getquill.context.SplicingBehavior
import io.getquill.auto.AutoDerive.Implicitly.*
//import io.getquill.MirrorContext.Codec.nullChecker
//import io.getquill.MirrorContext.Codec.stringDecoder
//import io.getquill.MirrorContext.Codec.intDecoder
import io.getquill.MirrorContext.Codec.*
import org.scalatest.freespec.AnyFreeSpec

object SomeSanePerson {
  case class Person(name: String, age: Int)
}

class SimpleMapSqlSanityTest extends AnyFreeSpec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)

  case class SanePerson(name: String, age: Int)

  //given SplicingBehaviorHint with {
  //  override type BehaviorType = SplicingBehavior.FailOnDynamic
  //}

  case class Foo(name: String)

  "simple test for one inline query converted to sql" in {
    //inline def q = quote {
    //  query[SanePerson]
    //}
    inline def qq = quote {
      query[SomeSanePerson.Person] //.map(p => p.name)
    }
    //val quat = quatOf[SanePerson]
    //qq.ast mustEqual Map(Entity("SanePerson", List(), quat.probit), Ident("p", quat), Property(Ident("p", quat), "name"))

    //import ctx._
    val output = ctx.run(qq).string //
    println(output)
    //output mustEqual """SELECT p.name FROM SanePerson p"""
  }

}
