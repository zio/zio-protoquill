package io.getquill

import scala.language.implicitConversions
import io.getquill.Dsl._
import io.getquill.Quoted
import io.getquill._
import io.getquill._
import io.getquill.ast._
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic

class BatchActionTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  "batch insert should work with" - {
    "simple case" in {
      val people = List(Person("Joe", 123), Person("Jill", 456))
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insert(p)) }

      mirror.triple mustEqual (
        "INSERT INTO Person (name,age) VALUES (?, ?)",
        List(List("Joe", 123), List("Jill", 456)),
        Static
      )
    }
  }
}