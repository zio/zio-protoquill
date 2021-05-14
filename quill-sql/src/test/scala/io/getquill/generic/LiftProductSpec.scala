package io.getquill.generic

import io.getquill.ast._
import io.getquill.quat.Quat
import io.getquill.generic.ElaborateStructureExt.TaggedSplicedCaseClass
import scala.quoted.Expr
import org.scalatest._
import io.getquill.context.ExecutionType
import io.getquill.context.mirror.Row
import io.getquill._

class LiftScalarSpec extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  "lift of scalar list should work for" - {
    val names = List("Joe", "Jack")
    "static case" in {
      inline def q = quote {
        query[Person].filter(p => liftQuery(names).contains(p.name))
      }
      ctx.run(q).triple mustEqual ("SELECT p.name, p.age FROM Person p WHERE p.name IN (?, ?)", List("Joe", "Jack"), ExecutionType.Static)
    }

    val otherNames = List("John", "James")
    "static case with other lifts" in {
      inline def q = quote {
        query[Person].filter(p => liftQuery(names).contains(p.name) || p.name == lift("Randal") || liftQuery(otherNames).contains(p.name) || p.name == lift("Ronald"))
      }
      ctx.run(q).triple mustEqual ("SELECT p.name, p.age FROM Person p WHERE p.name IN (?, ?) OR p.name = ? OR p.name IN (?, ?) OR p.name = ?", List("Joe", "Jack", "Randal", "John", "James", "Ronald"), ExecutionType.Static)
    }

    "dynamic case" in {
      val q = quote {
        query[Person].filter(p => liftQuery(names).contains(p.name))
      }
      ctx.run(q).triple mustEqual ("SELECT p.name, p.age FROM Person p WHERE p.name IN (?, ?)", List("Joe", "Jack"), ExecutionType.Dynamic)
    }

    "dynamic case with other lifts" in {
      val q = quote {
        query[Person].filter(p => liftQuery(names).contains(p.name) || p.name == lift("Randal") || liftQuery(otherNames).contains(p.name) || p.name == lift("Ronald"))
      }
      ctx.run(q).triple mustEqual ("SELECT p.name, p.age FROM Person p WHERE p.name IN (?, ?) OR p.name = ? OR p.name IN (?, ?) OR p.name = ?", List("Joe", "Jack", "Randal", "John", "James", "Ronald"), ExecutionType.Dynamic)
    }
  }
}

class LiftProductSpec extends Spec with Inside {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  // extension (ccl: CaseClassLift)
  //   def pullout = (ccl.ast, ccl.lifts)  

  // "lift of product should work for" - {
  //   "simple class" in {
  //     case class Person(name: String, age: Int)
  //     val v = Person("Joe", 123)
  //     val q = quote { lift(v) }
  //     inside(q) {
  //       case Quoted(
  //         CaseClass(List(("name", ScalarTag(idA)), ("age", ScalarTag(idB)))),
  //         List(EagerPlanter("Joe", _, idA1), EagerPlanter(123, _, idB1)),
  //         _
  //       ) =>
  //         idA mustEqual idA1
  //         idB mustEqual idB1
  //     }
  //   }
  // }

  "run function for lifted case class should work" - {
    // "simple class" in {
    //   case class Person(name: String, age: Int)
    //   val v = Person("Joe", 123)
    //   inline def q = quote { query[Person].insert(lift(v)) }
    //   ctx.run(q).triple mustEqual
    //     ("INSERT INTO Person (name,age) VALUES (?, ?)", List("Joe", 123), ExecutionType.Static)
    // }

    // "simple class - dynamic" in {
    //   case class Person(name: String, age: Int)
    //   val v = Person("Joe", 123)
    //   val q = quote { query[Person].insert(lift(v)) }
    //   ctx.run(q).triple mustEqual
    //     ("INSERT INTO Person (name,age) VALUES (?, ?)", List("Joe", 123), ExecutionType.Dynamic)
    // }

    "triple nested class" in {
      case class ReallyNested(one: Int, two: Int)
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val v = Entity("foo", Some(Nested(1, Some(ReallyNested(2, 3)))))
      inline def q = quote { query[Entity].insert(lift(v)) }
      val result = ctx.run(q) //hello
      // When fully expanded, the inner row-values for insertions are based on a tuple-index
      result.prepareRow.asInstanceOf[Row].data.toList mustEqual
        List(("_1","foo"), ("_2",Some(("_1",1))), ("_3",Some(("_1",2))), ("_4",Some(("_1",3))))
      result.string mustEqual 
        "INSERT INTO Entity (a,i,one,two) VALUES (?, ?, ?, ?)"
      result.info.executionType mustEqual
        ExecutionType.Static
    }

    
  }
}