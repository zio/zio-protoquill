package io.getquill

import io.getquill.context.mirror.Row
import io.getquill.util.StringOps._
import io.getquill.context.ExecutionType
import io.getquill.util.debug.PrintMac

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

  val s = io.getquill.MirrorSession.default

  "MapFlicer should" - {
    "Splice on a flat object" in {
      val keys = Map("firstName" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonFlat].filterByKeys(keys)
      }
      val r = ctx.run(q)
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.firstName, p.lastName, p.age FROM PersonFlat p WHERE (p.firstName = ? OR ? IS NULL) AND (p.lastName = ? OR ? IS NULL) AND (cast(p.age as VARCHAR) = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }

    "Splice on a object with an optional field" in {
      val keys = Map("firstName" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonFlatOpt].filterByKeys(keys)
      }
      val r = ctx.run(q)
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.firstName, p.lastName, p.age FROM PersonFlatOpt p WHERE (p.firstName = ? OR ? IS NULL) AND (p.lastName = ? OR ? IS NULL) AND (cast(p.age as VARCHAR) = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }

    "Splice on a nested object" in {
      val keys = Map("first" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonNest].filterByKeys(keys)
      }
      val r = ctx.run(q)
      println("========= Prepares ======\n" + r.prepareRow)

      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.first, p.last, p.age FROM PersonNest p WHERE (p.first = ? OR ? IS NULL) AND (p.last = ? OR ? IS NULL) AND (cast(p.age as VARCHAR) = ? OR ? IS NULL)"
      lifts mustEqual List("Joe", "Joe", null, null, "123", "123")
      executionType mustEqual ExecutionType.Static
    }

    "Splice on a nested optional object" in {
      val keys = Map("first" -> "Joe", "age" -> "123")
      inline def q = quote {
        query[PersonNestOpt].filterByKeys(keys)
      }
      val r = ctx.run(q) //
      val (qry, lifts, executionType) = r.triple
      qry mustEqual "SELECT p.first, p.last, p.age FROM PersonNestOpt p WHERE (p.first = ? OR ? IS NULL) AND (p.last = ? OR ? IS NULL) AND (cast(p.age as VARCHAR) = ? OR ? IS NULL)"
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
      qry mustEqual "SELECT p.first, p.last, p.age FROM PersonNestOptField p WHERE (p.first = ? OR ? IS NULL) AND (p.last = ? OR ? IS NULL) AND (cast(p.age as VARCHAR) = ? OR ? IS NULL)"
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
        PersonFlat("Joe","Bloggs",123)
    }

    "Filter Nested columns" in {
      val columns = List("name")
      inline def q = quote {
        query[PersonNest].filterColumns(columns)
      }
      val r = ctx.run(q)
      r.string.collapseSpace mustEqual
        """|SELECT
          | CASE WHEN ? THEN p.first ELSE null END,
          | CASE WHEN ? THEN p.last ELSE null END,
          | CASE WHEN ? THEN p.age ELSE null END
          | FROM PersonNest p
          |""".stripMargin.collapseSpace

      r.extractor(Row("first" -> "Joe", "last" -> "Bloggs", "age" -> 123), s) mustEqual
        PersonNest(Name("Joe","Bloggs"),123)
    }
  }
}