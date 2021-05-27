package io.getquill.context.sql

import io.getquill.Spec
import io.getquill.generic.ArrayEncoding
import io.getquill._

trait ArrayOpsSpec extends Spec { //hello

  val ctx: SqlContext[_, _] with ArrayEncoding

  import ctx._

  case class ArrayOps(id: Int, numbers: Seq[Int])

  val entriesList = List(
    ArrayOps(1, List(1, 2, 3)),
    ArrayOps(2, List(1, 4, 5)),
    ArrayOps(3, List(1, 4, 6))
  )

  inline def entity = quote(query[ArrayOps])

  inline def insertEntries = quote {
    liftQuery(entriesList).foreach(e => entity.insert(e))
  }



  inline def personEntity = quote(query[Person]) //hello
  case class Person(name: String, age: Int, stuff: Vector[Int])
  val peopleList = List(Person("Joe", 123, Vector(1,2,3)), Person("Jack", 456, Vector(1,2,3)))
  inline def peopleEntries = quote {
    liftQuery(peopleList).foreach(p => personEntity.insert(p))
  }

  val p = Person("Joe", 123, Vector(1,2,3))
  inline def personInsert = quote {
    personEntity.insert(lift(p))
  }

  object `contains` {
    inline def idByContains(x: Int) = quote(entity.filter(_.numbers.contains(lift(x))).map(_.id))

    inline def `Ex 1 return all` = quote(idByContains(1))
    val `Ex 1 expected` = List(1, 2, 3)

    inline def `Ex 2 return 1` = quote(idByContains(3))
    val `Ex 2 expected` = List(1)

    inline def `Ex 3 return 2,3` = quote(idByContains(4))
    val `Ex 3 expected` = List(2, 3)

    inline def `Ex 4 return empty` = quote(idByContains(10))
    val `Ex 4 expected` = Nil
  }
}
