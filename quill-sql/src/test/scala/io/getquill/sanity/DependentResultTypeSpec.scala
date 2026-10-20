package io.getquill.sanity

import io.getquill._

class DependentResultTypeSpec extends Spec {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  trait Finder[T] {
    type Out
    inline def find: Out
  }

  inline given Finder[Person] with {
    type Out = Query[Person]
    inline def find: Query[Person] = query[Person].filter(p => p.age > 18)
  }

  inline def find[T](using finder: Finder[T]): finder.Out = finder.find

  "query returned by an inline given through a dependent result type" in {
    inline def q = quote { find[Person].map(p => p.name) }
    ctx.run(q).string mustEqual "SELECT p.name FROM Person p WHERE p.age > 18"
  }
}
