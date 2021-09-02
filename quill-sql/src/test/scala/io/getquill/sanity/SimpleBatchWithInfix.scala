package io.getquill.sanity

import io.getquill._

object SimpleBatchWithInfix extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  "batch must work with simple infix" in {
    case class Person[T](name: String, age: Int)
    val names = List("Joe", "Jack")
    inline def q = quote {
      query[Person[String]].filter(p =>
        liftQuery(names).contains(p.name) && infix"fun(${p.name})".pure.as[Boolean]
      )
    }
    ctx.run(q).triple mustEqual (
      "SELECT p.name, p.age FROM Person p WHERE p.name IN (?) AND fun(p.name)",
      List("Joe", "Jack"),
      io.getquill.context.ExecutionType.Static
    )
  }
}
