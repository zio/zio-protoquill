package io.getquill.context.jdbc.postgres

import io.getquill._
import org.scalatest.Inside

class VariableColumnsSpec extends Spec with Inside {
  val ctx = testContext
  import ctx._

  case class PersonT(id: Int, name: String, age: Int)
  case class AddressT(ownerId: Int, street: String)
  case class Combo(name: String, street: Option[String])

  "Variable Columns Splicing" - {
    inline def q(inline columns: List[String]) =
      quote {
        query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
          .map((p, a) => Combo(p.name, a.map(_.street)))
          .filterColumns(columns)
      }
    inline def plan(inline columns: List[String]) =
      quote { infix"EXPLAIN ${q(columns)}".pure.as[Query[String]] }

    /** Resulting Plan should be something like:
     * 1) Hash Right Join  (cost=3182.00..7466.00 rows=100000 width=37)
     * 2)  Hash Cond: (a.ownerid = p.id)
     * 3)  ->  Seq Scan on addresst a  (cost=0.00..1540.00 rows=100000 width=9)
     * 4)  ->  Hash  (cost=1541.00..1541.00 rows=100000 width=4)
     * 5)        ->  Seq Scan on persont p  (cost=0.00..1541.00 rows=100000 width=4)
     */
    "Regular" in {
      val columns = List[String]("street")
      inside(ctx.run(plan(columns), OuterSelectWrap.Never)) {
        case List(a, b, c, e, f) =>
          a must include("Hash Right Join")
          b must include("Hash Cond")
          c must include("Seq Scan on addresst a")
      }

      // Also check that the actual query works
      ctx.run(q(columns).take(5)) mustEqual
        List(Combo(null,Some("1")), Combo(null,Some("2")), Combo(null,Some("3")), Combo(null,Some("4")), Combo(null,Some("5")))
    }

    "Remove Table from Plan when Columns Not Needed" in {
      val columns = List[String]()
      val result = ctx.run(plan(columns), OuterSelectWrap.Never).head
      result must startWith("Seq Scan on persont p")
    }

    "Remove Table from Plan when Columns for other table Not Needed" in {
      val columns = List[String]("name")
      inside(ctx.run(plan(columns), OuterSelectWrap.Never)) {
        case List(result) =>
          result must startWith("Seq Scan on persont p")
      }

      // Also check that the actual query works
      ctx.run(q(columns).take(5)) mustEqual
        List(Combo("1",None), Combo("2",None), Combo("3",None), Combo("4",None), Combo("5",None))
    }
  }
}