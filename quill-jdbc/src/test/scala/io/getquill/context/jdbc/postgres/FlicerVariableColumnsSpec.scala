package io.getquill.context.jdbc.postgres

import io.getquill._
import org.scalatest.Inside

class FlicerVariableColumnsSpec extends Spec with Inside {
  val ctx = testContext
  import ctx._

  case class PersonT(id: Int, first: String, last: String, age: Int)
  case class AddressT(ownerId: Int, street: String)
  case class Combo(name: String, street: Option[String])

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

  "Query Plan should adjust accordingly when" - {
    inline def q(inline columns: List[String]) =
      quote {
        query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
          .map((p, a) => Combo(p.first, a.map(_.street)))
          .filterColumns(columns)
      }
    inline def plan(inline columns: List[String]) =
      quote { sql"EXPLAIN ${q(columns)}".pure.as[Query[String]] }

    "one column from the joined table is selected" in {
      val columns = List[String]("street")
      // Also check that the actual query works
      ctx.run(q(columns).take(5)) mustEqual
        List(Combo(null,Some("1")), Combo(null,Some("2")), Combo(null,Some("3")), Combo(null,Some("4")), Combo(null,Some("5")))
    }

    // I.e. if no columns from the joined table are selected and the joined table has a unique constraint on the joined
    // key, if no columns are selected from it, that table doesn't need to be scanned at all (this would not be the case
    // unless the columns are unique and it is an outer join. If it were an inner join we would need to scan the second
    // table to know what columns to filter out. Even if it were an outer join and the keys would not be unique, we
    // would still need to select it because it could have duplicate entries (in the 2nd table) as result of the join.
    // In order to get rid of this latter issue we need to make the foreign key column on the 2nd table unique. Note that
    // databases know all this which is why the plan changes accordingly).
    "no columns from joined table are selected" in {
      // Note that despite the fact that we are selecting from PersonT, the Combo class's column is `name` so that is what
      // we are get columns from.
      val columns = List[String]("name")
      inside(ctx.run(plan(columns), OuterSelectWrap.Never)) {
        case List(result) =>
          result must startWith("Seq Scan on persont p")
      }

      // Also check that the actual query works
      ctx.run(q(columns).take(5)) mustEqual
        List(Combo("1",None), Combo("2",None), Combo("3",None), Combo("4",None), Combo("5",None))
    }

    "no columns from any table are selected" in {
      val columns = List[String]()
      val result = ctx.run(plan(columns), OuterSelectWrap.Never).head
      result must startWith("Seq Scan on persont p")
    }
  }
}