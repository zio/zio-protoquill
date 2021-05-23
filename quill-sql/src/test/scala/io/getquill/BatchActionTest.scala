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
  // through the class. If the parameters are removed (i.e. used `val ctx: Context[_, _]`), the LoadObject will try to 
  // load the base-object `Idiom` because that is the minimal thing that the Dialect parameter needs 
  // (and it seems LoadObject in BatchQueryExecution does not yet know what the values of the _, _ in Context[_, _]
  // are supposed to be)
  val ctx: Context[D, N] 
  import ctx._

  case class Person(id: Int, name: String, age: Int)
  inline def insertPeople = quote((p: Person) => query[Person].insert(p))
}

class BatchActionTest extends Spec with Inside with SuperContext[PostgresDialect, Literal] {
  // Need to fully type this otherwise scala compiler thinks it's still just 'Context' from the super-class
  // and the extensions (m: MirrorContext[_, _]#BatchActionMirror) etc... classes in Spec don't match their types correctly
  val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)
  import ctx._

  val people = List(Person(1, "Joe", 123), Person(2, "Jill", 456))

  "batch action with returning should work with" - {
    // "insert - returning" in {
    //   val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insert(p).returning(p => p.id)) }
    //   mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?) RETURNING id", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
    // }
    // // Batch Update retunring, the most basic kind but not very useful
    // "update - returning" in {
    //   val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].update(p).returning(p => p.id)) }
    //   mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?) RETURNING id", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
    // }
    // more useful variant
    "update - returning" in {
      //val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).update(_.name -> p.name).returning(p => p.id)) }
      //val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].update(_.name -> p.name).returning(p => p.id)) }
      //val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].update(p)) }

      val mirror2 = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.name == p.name).update(_.name -> p.name)) }
      println(mirror2)
      //val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.name == p.name).update(_.name -> p.name)) }
      mirror2.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?) RETURNING id", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
    }
    // "update - returning" in {
    //   val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].delete.returning(p => p.id)) }
    //   mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?) RETURNING id", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
    // }

    // "insert - returningGenerated" in {
    //   val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insert(p).returningGenerated(p => p.id)) }
    //   mirror.triple mustEqual (
    //     "INSERT INTO Person (name,age) VALUES (?, ?) RETURNING id",
    //     // The ids should be removed from the lifts list since their corresponding columns are removed (i.e. in the expanded insert assignments)
    //     List(List(/*1,*/ "Joe", 123), List(/*2,*/ "Jill", 456)),
    //     Static
    //   )
    // }


  }

  // "batch insert should work with" - {
  //   "simple case" in {
  //     val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insert(p)) }
  //     mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?)", List(List(1, "Joe", 123), List(2, "Jill", 456)),Static)
  //   }

  //   "simple case from super" in {
  //     val mirror = ctx.run { liftQuery(people).foreach(p => insertPeople(p)) }
  //     mirror.triple mustEqual ("INSERT INTO Person (id,name,age) VALUES (?, ?, ?)", List(List(1, "Joe", 123), List(2, "Jill", 456)), Static)
  //   }
  // }
}