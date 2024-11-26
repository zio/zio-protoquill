package io.getquill.sanity

import io.getquill.*
import io.getquill.ast.*
import io.getquill.Quoted
import io.getquill.context.SplicingBehaviorHint
import io.getquill.context.SplicingBehavior

class SimplePrepareSpec extends Spec {
  val ctx = SqlMirrorContext(PostgresDialect, Literal)
  import ctx._

  given SplicingBehaviorHint with {
    override type BehaviorType = SplicingBehavior.FailOnDynamic
  }

  "prepare should work for" - {
    case class Person(name: String, age: Int)
    given CompositeDecoder[Person] = SqlMirrorContext.deriveComposite

    "query" in {
      inline def q = quote { query[Person] }
      val result = prepare(q)
      result.sql mustEqual "SELECT x.name, x.age FROM Person x"
    }

    "batch" in {
      val list = List(Person("Joe", 1), Person("Jack", 2))
      inline def q = quote { liftQuery(list).foreach(p => query[Person].insertValue(p)) }
      val result = prepare(q)
      result.groups.length mustEqual 1
      result.groups(0)._1 mustEqual "INSERT INTO Person (name,age) VALUES (?, ?)"
      result.groups(0)._2.map(_.data) mustEqual List(Seq(("_1", "Joe"), ("_2", 1)), Seq(("_1", "Jack"), ("_2", 2)))
    }
  }
}
