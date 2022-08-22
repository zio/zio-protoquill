package io.getquill

import io.getquill.context.mirror.Row
import io.getquill.util.StringOps._
import io.getquill.context.ExecutionType
import io.getquill.util.debug.PrintMac
import io.getquill.context.mirror.MirrorSession
import java.time.Instant
import java.time.ZonedDateTime
import java.time.ZoneId
import java.util.TimeZone
import java.time.LocalDate

class FlicersSpec extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class PersonFlat(firstName: String, lastName: String, age: Int)
  case class PersonFlatOpt(firstName: Option[String], lastName: String, age: Int)

  case class Name(first: String, last: String)
  case class PersonNest(name: Name, age: Int)
  case class PersonNestOpt(name: Option[Name], age: Int)

  case class NameOpt(first: Option[String], last: String)
  case class PersonNestOptField(name: Option[NameOpt], age: Int)

  val s = MirrorSession.default

  "MapFlicer should" - {
    "Splice on a flat object" in {
      val keys = Map("firstName" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonFlat].filterByKeys(keys)
      }
      val r = ctx.run(q)
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.firstName, p.lastName, p.age FROM PersonFlat p WHERE (p.firstName = ? OR ? IS NULL) AND (p.lastName = ? OR ? IS NULL) AND (p.age = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }

    "Support all encoding types" - {
      val now = LocalDate.of(2022, 1, 1)
      inline def q(inline keys: Map[String, Any]) = quote {
        query[ManyTypes].filterByKeys(keys)
      }

      case class ManyTypes(s: String, so: Option[String], i: Int, io: Option[Int], ld: LocalDate, ldo: Option[LocalDate])
      "Splice on an object multiple encoding types" in {
        val keys = Map[String, Any]("s" -> "Joe", "so" -> "Joe", "i" -> 123, "io" -> 123, "ld" -> now, "ldo" -> now)
        val r = ctx.run(q(keys))
        val (qry, lifts, executionType) = r.triple
        qry mustEqual "SELECT p.s, p.so, p.i, p.io, p.ld, p.ldo FROM ManyTypes p WHERE (p.s = ? OR ? IS NULL) AND (p.so = ? OR ? IS NULL) AND (p.i = ? OR ? IS NULL) AND (p.io = ? OR ? IS NULL) AND (p.ld = ? OR ? IS NULL) AND (p.ldo = ? OR ? IS NULL)"
        lifts mustEqual List("Joe", "Joe", "Joe", "Joe", 123, 123, 123, 123, now, now, now, now)
        executionType mustEqual ExecutionType.Static
      }
      "Splice on an object multiple encoding types - missing Nones" in {
        val keys = Map[String, Any]("s" -> "Joe", "i" -> 123, "ld" -> now)
        val r = ctx.run(q(keys))
        val (qry, lifts, executionType) = r.triple
        qry mustEqual "SELECT p.s, p.so, p.i, p.io, p.ld, p.ldo FROM ManyTypes p WHERE (p.s = ? OR ? IS NULL) AND (p.so = ? OR ? IS NULL) AND (p.i = ? OR ? IS NULL) AND (p.io = ? OR ? IS NULL) AND (p.ld = ? OR ? IS NULL) AND (p.ldo = ? OR ? IS NULL)"
        lifts mustEqual List("Joe", "Joe", null, null, 123, 123, null, null, now, now, null, null)
        executionType mustEqual ExecutionType.Static
      }
      "Splice on an object multiple encoding types - missing All" in {
        val keys = Map[String, Any]("s" -> "Joe", "i" -> 123, "ld" -> now)
        val r = ctx.run(q(keys))
        val (qry, lifts, executionType) = r.triple
        qry mustEqual "SELECT p.s, p.so, p.i, p.io, p.ld, p.ldo FROM ManyTypes p WHERE (p.s = ? OR ? IS NULL) AND (p.so = ? OR ? IS NULL) AND (p.i = ? OR ? IS NULL) AND (p.io = ? OR ? IS NULL) AND (p.ld = ? OR ? IS NULL) AND (p.ldo = ? OR ? IS NULL)"
        lifts mustEqual List("Joe", "Joe", null, null, 123, 123, null, null, now, now, null, null)
        executionType mustEqual ExecutionType.Static
      }
    }

    "Splice on a object with an optional field" in {
      val keys = Map("firstName" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonFlatOpt].filterByKeys(keys)
      }
      val r = ctx.run(q)
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.firstName, p.lastName, p.age FROM PersonFlatOpt p WHERE (p.firstName = ? OR ? IS NULL) AND (p.lastName = ? OR ? IS NULL) AND (p.age = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }

    "Splice on a nested object" in {
      val keys = Map("first" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonNest].filterByKeys(keys)
      }
      val r = ctx.run(q)
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.first, p.last, p.age FROM PersonNest p WHERE (p.first = ? OR ? IS NULL) AND (p.last = ? OR ? IS NULL) AND (p.age = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }

    "Splice on a nested optional object" in {
      val keys = Map("first" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonNestOpt].filterByKeys(keys)
      }
      val r = ctx.run(q)
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.first, p.last, p.age FROM PersonNestOpt p WHERE (p.first = ? OR ? IS NULL) AND (p.last = ? OR ? IS NULL) AND (p.age = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }

    "Splice on a nested optional object with a optional field" in {
      val keys = Map("first" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonNestOptField].filterByKeys(keys)
      }
      val r = ctx.run(q) //
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.first, p.last, p.age FROM PersonNestOptField p WHERE (p.first = ? OR ? IS NULL) AND (p.last = ? OR ? IS NULL) AND (p.age = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }
  }

  "ListFlicer should" - {

    "Filter Standard columns" in {
      val columns = List("firstName")
      inline def q = quote {
        query[PersonFlat].filterColumns(columns)
      }
      val r = ctx.run(q)
      r.extractor(Row("firstName" -> "Joe", "lastName" -> "Bloggs", "age" -> 123), s) mustEqual
        PersonFlat("Joe", "Bloggs", 123)
    }

    "Filter Nested columns" in {
      val columns = List("name")
      inline def q = quote {
        query[PersonNest].filterColumns(columns)
      }
      val r = ctx.run(q)
      r.string.collapseSpace mustEqual
        """|SELECT
           | CASE WHEN ? THEN p.first ELSE null END AS first,
           | CASE WHEN ? THEN p.last ELSE null END AS last,
           | CASE WHEN ? THEN p.age ELSE null END AS age
           | FROM PersonNest p
           |""".stripMargin.collapseSpace

      r.extractor(Row("first" -> "Joe", "last" -> "Bloggs", "age" -> 123), s) mustEqual
        PersonNest(Name("Joe", "Bloggs"), 123)
    }
  }
}
