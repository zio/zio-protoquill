package io.getquill

import io.getquill.MirrorContext.*

class FinalValSpec extends Spec {
  case class Person(name: String, age: Int)
  given MirrorContext.CompositeDecoder[Person] = MirrorContext.deriveComposite[Person]

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._


  final val maxAge = 21

  "final val should translate to static constant" in {
    inline def q = quote {
      query[Person].filter(p => p.age > maxAge)
    }
    ctx.run(q).string mustEqual "SELECT p.name, p.age FROM Person p WHERE p.age > 21"
  }

}
