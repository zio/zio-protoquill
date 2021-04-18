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

  case class Person(name: String, age: Int)
  inline def insertPeople = quote((p: Person) => query[Person].insert(p))
}

class BatchActionTest extends Spec with Inside with SuperContext[MirrorSqlDialect, Literal] {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  "batch insert should work with" - {
    val people = List(Person("Joe", 123), Person("Jill", 456))

    // "simple case" in {
    //   val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].insert(p)) }

    //   mirror.triple mustEqual (
    //     "INSERT INTO Person (name,age) VALUES (?, ?)",
    //     List(List("Joe", 123), List("Jill", 456)),
    //     Static
    //   )
    // }

    "simple case from super" in {
      val mirror = ctx.run { liftQuery(people).foreach(p => insertPeople(p)) }

      //import io.getquill.util.debug.PrintMac
      //PrintMac { liftQuery(people).foreach(p => insertPeople(p)) }
    } //hello
  }
}