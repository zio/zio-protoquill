package io.getquill.sanity

import io.getquill._
import io.getquill.context.SplicingBehaviorHint
import io.getquill.context.SplicingBehavior

object SimpleBatchWithInfix extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  given SplicingBehaviorHint with
    override type BehaviorType = SplicingBehavior.FailOnDynamic

  "batch must work with simple sql" in {
    case class Person[T](name: String, age: Int)
    val names = List("Joe", "Jack")
    inline def q = quote {
      query[Person[String]].filter(p =>
        liftQuery(names).contains(p.name) && sql"fun(${p.name})".pure.as[Boolean]
      )
    }
    ctx.run(q).triple mustEqual (
      "SELECT p.name, p.age FROM Person p WHERE p.name IN (?) AND fun(p.name)",
      List("Joe", "Jack"),
      io.getquill.context.ExecutionType.Static
    )
  }
}
