package io.getquill.context.jdbc.postgres

import io.getquill._
import org.scalatest.Inside

class FlicerCombinationSpec extends Spec with Inside {
  val ctx = testContext
  import ctx._

  case class PersonT(id: Int, first: String, last: String, age: Int)
  case class AddressT(ownerId: Int, street: String)
  case class Combo(name: String, street: Option[String])
  case class PersonAddress(first: String, last: String, street: Option[String])

  override def beforeAll(): Unit = {
    ctx.run(sql"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
    // Using sequence generation in the DB to create a table with a large amount of content fast. Otherwise
    // the test has to wait for 1000000 individual inserts which is very slow.
    ctx.run(sql"insert into PersonT (first, last, age) select i, i, i from generate_series(1, 100000) as t(i);".as[Insert[PersonT]])
    ctx.run(sql"insert into AddressT (ownerId, street) select i, i from generate_series(1, 100000) as t(i);".as[Insert[PersonT]])
  }

  override def afterAll(): Unit = {
    // Want to truncate instead of delete so that plan cost will be consistent
    ctx.run(sql"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
  }

  "Selection should be correct when" - {
    inline def q(inline columns: List[String], inline keys: Map[String, String]) =
      quote {
        query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
          .map((p, a) => PersonAddress(p.first, p.last, a.map(_.street)))
          .filterByKeys(keys)
          .filterColumns(columns)
          .take(10)
      }
    "Keys used for filtration are included in the selection output - and they are out of order" in {
      ctx.run(q(List("last", "first", "street"), Map("first" -> "1"))) mustEqual List(PersonAddress("1","1",Some("1")))
    }
    "Keys used for filtration are NOT included in the selection output" in {
      ctx.run(q(List("last", "street"), Map("first" -> "1"))) mustEqual List(PersonAddress(null,"1",Some("1")))
    }
    "Keys used for filtration are NOT included in the selection output and only one table is selected" in {
      ctx.run(q(List("last"), Map("first" -> "1"))) mustEqual List(PersonAddress(null,"1",None))
    }
  }
}