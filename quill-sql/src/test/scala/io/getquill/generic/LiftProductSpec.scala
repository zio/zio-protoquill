package io.getquill.generic

import io.getquill.ast._
import io.getquill.Spec
import io.getquill.quat.Quat
import io.getquill.generic.ElaborateStructureExt.TaggedSplicedCaseClass
import scala.quoted.Expr
import io.getquill.Dsl._
import io.getquill._
import io.getquill._
import org.scalatest._
import io.getquill.context.ExecutionType
import io.getquill.context.mirror.Row

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

    "triple nested class" in {
      case class ReallyNested(one: Int, two: Int)
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val v = Entity("foo", Some(Nested(1, Some(ReallyNested(2, 3)))))
      inline def q = quote { query[Entity].insert(lift(v)) }
      val result = ctx.run(q)
      // When fully expanded, the inner row-values for insertions are based on a tuple-index
      result.prepareRow.asInstanceOf[Row].data.toList mustEqual
        List(("_1","foo"), ("_2",Some(("_1",1))), ("_3",Some(("_1",2))), ("_4",Some(("_1",3))))
      result.string mustEqual 
        "INSERT INTO Entity (a,i,one,two) VALUES (?, ?, ?, ?)"
      result.executionType mustEqual
        ExecutionType.Static
    }

    
  }
}