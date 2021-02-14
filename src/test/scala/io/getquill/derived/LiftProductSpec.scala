package io.getquill.derived

import io.getquill.ast._
import io.getquill.Spec
import io.getquill.quat.Quat
import io.getquill.derived.ElaborateStructureExt.TaggedSplicedCaseClass
import scala.quoted.Expr
import io.getquill.quoter.Dsl._
import io.getquill.quoter._
import io.getquill._
import org.scalatest._
import io.getquill.context.ExecutionType

class LiftProductSpec extends Spec with Inside {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  // extension (ccl: CaseClassLift)
  //   def pullout = (ccl.ast, ccl.lifts)  

  "lift of product should work for" - {
    "simple class" in {
      case class Person(name: String, age: Int)
      val v = Person("Joe", 123)
      val q = quote { lift(v) }
      inside(q) {
        case Quoted(
          CaseClass(List(("name", ScalarTag(idA)), ("age", ScalarTag(idB)))),
          List(EagerPlanter("Joe", _, idA1), EagerPlanter(123, _, idB1)),
          _
        ) =>
          idA mustEqual idA1
          idB mustEqual idB1
      }
    }
    // TODO Tests for more complex cases
  }

  "run function for lifted case class should work" - {
    "simple class" in {
      case class Person(name: String, age: Int)
      val v = Person("Joe", 123)
      inline def q = quote { query[Person].insert(lift(v)) }
      ctx.run(q).triple mustEqual
        ("INSERT INTO Person (name,age) VALUES (?, ?)", List("Joe", 123), ExecutionType.Static)
    }

    "simple class - dynamic" in {
      case class Person(name: String, age: Int)
      val v = Person("Joe", 123)
      val q = quote { query[Person].insert(lift(v)) }
      ctx.run(q).triple mustEqual
        ("INSERT INTO Person (name,age) VALUES (?, ?)", List("Joe", 123), ExecutionType.Dynamic)
    }

    
  }
}