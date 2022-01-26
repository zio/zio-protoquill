package io.getquill

import scala.language.implicitConversions

import io.getquill.Quoted
import io.getquill.ast._
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.context.Context
import io.getquill.quote
import io.getquill.query

trait SuperContext[D <: io.getquill.idiom.Idiom, N <: NamingStrategy] {
  // Need SqlContext here otherwise there will be encoder-not-found issues in 'insertPeople' since that does lifting
  // Also note that the context needs to be typed. As an example of how to do that, we passed typing parameters
  // through the class. If the parameters are removed (i.e. used `val ctx: Context[_, _]`), the LoadModule will try to
  // load the base-object `Idiom` because that is the minimal thing that the Dialect parameter needs
  // (and it seems LoadModule in BatchQueryExecution does not yet know what the values of the _, _ in Context[_, _]
  // are supposed to be)
  val ctx: Context[D, N]
  import ctx._

  case class Person(id: Int, name: String, age: Int)
  inline def insertPeople = quote((p: Person) => query[Person].insertValue(p))
  val insertPeopleDynamic = quote((p: Person) => query[Person].insertValue(p))
}

class BatchActionTest extends Spec with Inside with SuperContext[PostgresDialect, Literal] {
  // Need to fully type this otherwise scala compiler thinks it's still just 'Context' from the super-class
  // and the extensions (m: MirrorContext[_, _]#BatchActionMirror) etc... classes in Spec don't match their types correctly
  val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)
  import ctx._

  val people = List(Person(1, "Joe", 123), Person(2, "Jill", 456))

  "batch action with returning should work with" - {
    "insert - returning" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insertValue(p).returning(p => p.id)) }
      mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?) RETURNING id", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
    }
    "insert - returningGenerated" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insertValue(p).returningGenerated(p => p.id)) }
      mirror.triple mustEqual (
        "INSERT INTO Person (name,age) VALUES (?, ?) RETURNING id",
        // The ids should be removed from the lifts list since their corresponding columns are removed (i.e. in the expanded insert assignments)
        List(List(/*1,*/ "Joe", 123), List(/*2,*/ "Jill", 456)),
        Static
      )
    }

    // update returning with filter, not very useful but good baseline
    "update - returning" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).updateValue(p).returning(p => p.id)) }
      mirror.triple mustEqual ("UPDATE Person SET id = ?, name = ?, age = ? WHERE id = ? RETURNING id", List(List(1, "Joe", 123, 1), List(2, "Jill", 456, 2)), Static)
    }

    // TODO dsl does not support this yet but would be quite useful
    //"update - returningGenerated" in {
    //  val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).updateValue(p).returningGenerated(p => p.id)) }
    //  //mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?) RETURNING id", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
    //}
  }

  "batch action should work with" - {
    "dynamic splice" in {
      val q = quote {
        liftQuery(people).foreach(p => query[Person].insertValue(p))
      }
      val mirror = ctx.run(q)
      mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?)", List(List(1, "Joe", 123), List(2, "Jill", 456)),Dynamic)
    }

    "insert" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insertValue(p)) }
      mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?)", List(List(1, "Joe", 123), List(2, "Jill", 456)),Static)
    }

    "insert with function splice" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => insertPeople(p)) }
      mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?)", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
    }

    "insert with dynamic function splice" in { // I.e. splicing the insertPeopleDynamic segment should make the whole query dynamic... and it should still work
      val mirror = ctx.run { liftQuery(people).foreach(p => insertPeopleDynamic(p)) }
      mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?)", List(List(1, "Joe", 123), List(2, "Jill", 456)), Dynamic)
    }

    "update" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).update(_.name -> p.name, _.age -> p.age)) }
      mirror.triple mustEqual ("UPDATE Person SET name = ?, age = ? WHERE id = ?", List(List("Joe", 123, 1), List("Jill", 456, 2)), Static)
    }

    "update - object with meta" in {
      inline given UpdateMeta[Person] = updateMeta(_.id)
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).updateValue(p)) }
      mirror.triple mustEqual (
        "UPDATE Person SET name = ?, age = ? WHERE id = ?",
        List(List("Joe", 123, 1), List("Jill", 456, 2)),
        Static
      )
    }

    "delete" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).delete) }
      mirror.triple mustEqual ("DELETE FROM Person WHERE id = ?", List(List(1), List(2)), Static)
    }
  }
}
