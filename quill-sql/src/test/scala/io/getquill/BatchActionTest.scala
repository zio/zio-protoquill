package io.getquill

import io.getquill.ast.*
import io.getquill.context.ExecutionType.{Dynamic, Static}
import io.getquill.context.{Context, ExecutionType}
import io.getquill.quat.quatOf
import io.getquill.util.debug.PrintMac
import io.getquill.{QuotationLot, QuotationVase, Quoted, query, quote}
import org.scalatest.*

import scala.language.implicitConversions

trait SuperContext[D <: io.getquill.idiom.Idiom, N <: NamingStrategy] {
  // Need SqlContext here otherwise there will be encoder-not-found issues in 'insertPeople' since that does lifting
  // Also note that the context needs to be typed. As an example of how to do that, we passed typing parameters
  // through the class. If the parameters are removed (i.e. used `val ctx: Context[_, _]`), the LoadModule will try to
  // load the base-object `Idiom` because that is the minimal thing that the Dialect parameter needs
  // (and it seems LoadModule in QueryExecutionBatch does not yet know what the values of the _, _ in Context[_, _]
  // are supposed to be)
  val ctx: Context[D, N] //

  import ctx.*

  enum Sex {
    case Male, Female
  }

  object Sex {
    given encoder: MappedEncoding[Sex, String] =
      MappedEncoding[Sex, String] {
        case Sex.Male => "male"
        case Sex.Female => "female"
      }

    given decoder: MappedEncoding[String, Sex] =
      MappedEncoding[String, Sex] {
        case "male" => Sex.Male
        case "female" => Sex.Female
      }
  }

  case class Person(id: Int, name: String, age: Int, sex: Sex)

  inline def insertPeople = quote((p: Person) => query[Person].insertValue(p))

  val insertPeopleDynamic = quote((p: Person) => query[Person].insertValue(p))

  inline def updatePeopleById = quote((p: Person) => query[Person].filter(pt => pt.id == p.id).updateValue(p))

  val updatePeopleByIdDynamic = quote((p: Person) => query[Person].filter(pt => pt.id == p.id).updateValue(p))
}

class BatchActionTest extends Spec with Inside with SuperContext[MirrorSqlDialectWithReturnClause, Literal] {
  // Need to fully type this otherwise scala compiler thinks it's still just 'Context' from the super-class
  // and the extensions (m: MirrorContext[_, _]#BatchActionMirror) etc... classes in Spec don't match their types correctly
  val ctx: MirrorContext[MirrorSqlDialectWithReturnClause, Literal] = new MirrorContext[MirrorSqlDialectWithReturnClause, Literal](MirrorSqlDialectWithReturnClause, Literal)

  import ctx.*

  val people = List(Person(1, "Joe", 123, Sex.Male), Person(2, "Jill", 456, Sex.Female))

  "batch action with returning should work with" - {
    "insert - returning" in {
      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].insertValue(p).returning(p => p.id))
      }
      mirror.triple mustEqual("INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?) RETURNING id", List(List(1, "Joe", 123, "male"), List(2, "Jill", 456, Sex.Female)), Static)
    }
    "insert - returningGenerated" in {
      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].insertValue(p).returningGenerated(p => p.id))
      }
      mirror.triple mustEqual(
        "INSERT INTO Person (name,age,sex) VALUES (?, ?, ?) RETURNING id",
        // The ids should be removed from the lifts list since their corresponding columns are removed (i.e. in the expanded insert assignments)
        List(List(/*1,*/ "Joe", 123, "male"), List(/*2,*/ "Jill", 456, "female")),
        Static
      )
    }

    // update returning with filter, not very useful but good baseline
    "update - returning" in {
      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).updateValue(p).returning(p => p.id))
      }
      mirror.triple mustEqual("UPDATE Person AS pf SET id = ?, name = ?, age = ?, sex = ? WHERE pf.id = ? RETURNING id", List(List(1, "Joe", 123, "male", 1), List(2, "Jill", 456, "female", 2)), Static)
    }

    // TODO dsl does not support this yet but would be quite useful
    // "update - returningGenerated" in {
    //  val mirror = ctx.run { liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).updateValue(p).returningGenerated(p => p.id)) }
    //  //mirror.triple mustEqual ("INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?) RETURNING id", List(List(1, "Joe", 123, "male"), List(2, "Jill", 456, "female")), Static)
    // }
  }

  "batch action should work with" - {
    "dynamic splice" in {
      val q = quote {
        liftQuery(people).foreach(p => query[Person].insertValue(p))
      }
      val mirror = ctx.run(q)
      mirror.triple mustEqual("INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)", List(List(1, "Joe", 123, "male"), List(2, "Jill", 456, "female")), Dynamic)
    }

    "insert" in {
      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].insertValue(p))
      }
      mirror.triple mustEqual("INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)", List(List(1, "Joe", 123, "male"), List(2, "Jill", 456, "female")), Static)
    }

    case class Vip(vipId: Int, vipName: String, vipAge: Int, vipSex: Sex, other: String)
    "insert - different-objects" in {
      val vips = List(Vip(1, "Joe", 123, Sex.Male, "Something"), Vip(2, "Jill", 456, Sex.Female, "Something"))
      val mirror = ctx.run {
        liftQuery(vips).foreach(v => query[Person].insertValue(Person(v.vipId, v.vipName, v.vipAge, v.vipSex)))
      }
      mirror.triple mustEqual("INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)", List(List(1, "Joe", 123, "male"), List(2, "Jill", 456, "female")), Static)
    }

    "update - liftQuery scalars" in {
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => query[Person].filter(p => p.id == i).update(_.age -> 111))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = 111 WHERE p.id = ?", List(List(1), List(2), List(3)), Static)
    }

    "update - liftQuery scalars - dynamic" in {
      val updateDynamic = quote {
        (i: Int) => query[Person].filter(p => p.id == i).update(_.age -> 111)
      }
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => updateDynamic(i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = 111 WHERE p.id = ?", List(List(1), List(2), List(3)), Dynamic)
    }

    "update - extra lift" in {
      // Future UseCase Note a filtered-insert does not make sense this way, should have a specific warning about it (i.e. that it's not supported because it's a filtered insert)
      // val mirror = ctx.run { query[Person].filter(p => p.id == 123).insertValue(people(0)) }
      //
      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].filter(p => p.id == lift(36)).updateValue(p))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET id = ?, name = ?, age = ?, sex = ? WHERE p.id = ?", List(List(1, "Joe", 123, "male", 36), List(2, "Jill", 456, "female", 36)), Static)
    }

    "update - extra lift + scalars" in {
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => query[Person].filter(p => p.id == lift(36)).update(_.age -> i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id = ?", List(List(1, 36), List(2, 36), List(3, 36)), Static)
    }

    "update - extra lift + scalars + multi-use" in {
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => query[Person].filter(p => p.id == i && p.age == lift(123)).update(_.age -> i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id = ? AND p.age = ?", List(List(1, 1, 123), List(2, 2, 123), List(3, 3, 123)), Static)
    }

    "update - extra lift + scalars + liftQuery/setContains" in {
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => query[Person].filter(p => liftQuery(List(36, 49)).contains(p.id)).update(_.age -> i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id IN (?, ?)", List(List(1, 36, 49), List(2, 36, 49), List(3, 36, 49)), Static)
    }

    "update - extra lift + scalars + liftQuery/setContains + others" in {
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => query[Person].filter(p => liftQuery(List(36, 49)).contains(p.id) && p.id == lift(789)).update(_.age -> i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id IN (?, ?) AND p.id = ?", List(List(1, 36, 49, 789), List(2, 36, 49, 789), List(3, 36, 49, 789)), Static)
    }

    "update - extra lift - dynamic" in {
      val updateDynamic = quote {
        (p: Person) => query[Person].filter(p => p.id == lift(36)).updateValue(p)
      }
      val mirror = ctx.run {
        liftQuery(people).foreach(p => updateDynamic(p))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET id = ?, name = ?, age = ? WHERE p.id = ?", List(List(1, "Joe", 123, 36), List(2, "Jill", 456, 36)), Dynamic)
    }

    "update - extra lift - dynamic + scalars" in {
      val updateDynamic = quote {
        (i: Int) => query[Person].filter(p => p.id == lift(36)).update(_.age -> i)
      }
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => updateDynamic(i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id = ?", List(List(1, 36), List(2, 36), List(3, 36)), Dynamic)
    }

    "update - extra lift - dynamic + scalars + multi-use" in {
      val updateDynamic = quote {
        (i: Int) => query[Person].filter(p => p.id == i && p.age == lift(123)).update(_.age -> i)
      }
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => updateDynamic(i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id = ? AND p.age = ?", List(List(1, 1, 123), List(2, 2, 123), List(3, 3, 123)), Dynamic)
    }

    "update - extra lift - dynamic + scalars + liftQuery/setContains" in {
      val updateDynamic = quote {
        (i: Int) => query[Person].filter(p => liftQuery(List(36, 49)).contains(p.id)).update(_.age -> i)
      }
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => updateDynamic(i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id IN (?, ?)", List(List(1, 36, 49), List(2, 36, 49), List(3, 36, 49)), Dynamic)
    }

    "update - extra lift - dynamic + scalars + liftQuery/setContains + others" in {
      val updateDynamic = quote {
        (i: Int) => query[Person].filter(p => liftQuery(List(36, 49)).contains(p.id) && p.id == lift(789)).update(_.age -> i)
      }
      val mirror = ctx.run {
        liftQuery(List(1, 2, 3)).foreach(i => updateDynamic(i))
      }
      mirror.triple mustEqual("UPDATE Person AS p SET age = ? WHERE p.id IN (?, ?) AND p.id = ?", List(List(1, 36, 49, 789), List(2, 36, 49, 789), List(3, 36, 49, 789)), Dynamic)
    }

    case class MyPerson(id: Int, name: String, birthYear: Int)
    "update via tuple" in {
      val birthYearUpdates = List((3431, 1983), (2976, 1972), (1511, 1991)) // // /// // // //
      val a = ctx.run {
        liftQuery(birthYearUpdates).foreach {
          case (id, year) =>
            query[MyPerson].filter(p => p.id == id).update(p => p.birthYear -> year)
        }
      }
      a.triple mustEqual("UPDATE MyPerson AS p SET birthYear = ? WHERE p.id = ?", List(List(1983, 3431), List(1972, 2976), List(1991, 1511)), Static)

      val b = ctx.run {
        liftQuery(birthYearUpdates).foreach((id, year) =>
          query[MyPerson].filter(p => p.id == id).update(p => p.birthYear -> year)
        )
      }
      b.triple mustEqual("UPDATE MyPerson AS p SET birthYear = ? WHERE p.id = ?", List(List(1983, 3431), List(1972, 2976), List(1991, 1511)), Static)
    }

    "update via tuple - dynamic" in {
      val updateDynamic = quote {
        (id: Int, year: Int) => query[MyPerson].filter(p => p.id == id).update(p => p.birthYear -> year)
      }

      val birthYearUpdates = List((3431, 1983), (2976, 1972), (1511, 1991))
      val a = ctx.run {
        liftQuery(birthYearUpdates).foreach {
          case (id, year) => updateDynamic(id, year)
        }
      }
      a.triple mustEqual("UPDATE MyPerson AS p SET birthYear = ? WHERE p.id = ?", List(List(1983, 3431), List(1972, 2976), List(1991, 1511)), Dynamic)

      val b = ctx.run {
        liftQuery(birthYearUpdates).foreach((id, year) => updateDynamic(id, year))
      }
      b.triple mustEqual("UPDATE MyPerson AS p SET birthYear = ? WHERE p.id = ?", List(List(1983, 3431), List(1972, 2976), List(1991, 1511)), Dynamic)

      // Does not work, variable tracking has an issue
      // val b = ctx.run {
      //   liftQuery(birthYearUpdates).foreach(updateDynamic(_, _))
      // }
      // b.triple mustEqual ("UPDATE MyPerson SET birthYear = ? WHERE id = ?", List(List(1983, 3431), List(1972, 2976), List(1991, 1511)), Dynamic)
    }

    "insert with function splice" in {
      val mirror = ctx.run {
        liftQuery(people).foreach(p => insertPeople(p))
      }
      mirror.triple mustEqual("INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)", List(List(1, "Joe", "male", 123), List(2, "Jill", "female", 456)), Static)
    }

    "insert with dynamic function splice" in { // I.e. splicing the insertPeopleDynamic segment should make the whole query dynamic... and it should still work
      val mirror = ctx.run {
        liftQuery(people).foreach(p => insertPeopleDynamic(p))
      }
      mirror.triple mustEqual("INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)", List(List(1, "Joe", "male", 123), List(2, "Jill", "female", 456)), Dynamic)
    }

    "update" in {
      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).update(_.name -> p.name, _.age -> p.age))
      }
      mirror.triple mustEqual("UPDATE Person AS pf SET name = ?, age = ? WHERE pf.id = ?", List(List("Joe", 123, 1), List("Jill", 456, 2)), Static)
    }

    "update - object with meta" in {
      inline given UpdateMeta[Person] = updateMeta(_.id)

      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).updateValue(p))
      }
      mirror.triple mustEqual(
        "UPDATE Person AS pf SET name = ?, age = ?, sex = ? WHERE pf.id = ?",
        List(List("Joe", 123, "male", 1), List("Jill", 456, "female", 2)),
        Static
      )
    }

    "delete" in {
      val mirror = ctx.run {
        liftQuery(people).foreach(p => query[Person].filter(pf => pf.id == p.id).delete)
      }
      mirror.triple mustEqual("DELETE FROM Person AS pf WHERE pf.id = ?", List(List(1), List(2)), Static)
    }
  }
}
