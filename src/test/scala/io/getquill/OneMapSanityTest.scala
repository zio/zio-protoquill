package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.CoreDsl._
import io.getquill._
import io.getquill.ast._
import miniquill.quoter.Quoted

class OneMapSanityTest extends Spec {
  case class SanePerson(name: String, age: Int)

  "simple test for inline query and map" in {
    inline def q = quote {
      query[SanePerson] // hello
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
     qq.ast mustEqual Map(Entity("SanePerson", List()), Ident("p"), Property(Ident("p"), "name"))
  }

}
