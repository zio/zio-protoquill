package io.getquill.context.jdbc.postgres

import java.time.LocalDateTime
import io.getquill._

object MyTest {
  val context = testContext
  import context._

  def fun2() = {
    import extras._

    case class Person(id: Int, birthYear: Int, computedAge: Int)

    val birthYearUpdates = List((1, 1983), (2, 1972), (3, 1991))
    val a: List[Int] = context.run {
      liftQuery(birthYearUpdates).foreach {
        case (id, year) =>
          query[Person]
            .filter(p => p.id == id)
            .update(p => p.birthYear -> year)
            .returning(_.computedAge)
      }
    }
  }

  // case class ElectricCar(id: Long,name: String, lastChargeId: Option[Long])
  // case class Charge(id: Long, dateTime: LocalDateTime)
  /*
  CREATE TABLE ElectricCar(
    id int,
    name varchar,
    lastChargeId: int
  )
  CREATE TABLE Charge(
    id int,
    dateTime timestamp
  )
  */
  // select * from system s where organization_id = '1' and s.version = (select max(version) from system s2 where s.id = s2.id)
  // def fun(): Unit = {
  //   val r = run(
  //     query[ElectricCar]
  //       .leftJoin(query[Charge]).on((ec,c) => ec.lastChargeId.forall(_ == c.id))
  //   )
  //   println(r)
  // }

  def main(args: Array[String]): Unit = {}

  import extras._

  // sealed trait VersionFilter
  // object VersionFilter {
  //   case class Specific(v: Int) extends VersionFilter
  //   case object TotalMax extends VersionFilter
  // }



  // case class system(id: Int, version: Int)

  // inline def systemByVersion(inline filter: VersionFilter) =
  //   inline filter match
  //     case vs: VersionFilter.Specific =>
  //       query[system].filter(s => s.version == lift(vs.v))
  //     case VersionFilter.TotalMax =>
  //       query[system].filter(s => s.version ===
  //         query[system].filter(s2 => s2.id == s.id).map(_.version).max
  //       )

  // def fun1(): Unit = {
  //   run(
  //     systemByVersion(VersionFilter.Specific(123))
  //   )
  //   // SELECT s.ID AS id, s.VERSION AS version FROM SYSTEM s WHERE s.VERSION =
  //   //   (SELECT MAX(s2.VERSION) FROM SYSTEM s2 WHERE s2.ID = s.ID)

  //   // run(
  //   //   systemByVersion(s => s.version == 123)
  //   // )
  // }
}