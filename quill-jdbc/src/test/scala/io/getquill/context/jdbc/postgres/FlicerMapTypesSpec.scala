package io.getquill.context.jdbc.postgres

import io.getquill._
import org.scalatest.Inside
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.ZoneId

object FlicerMapTypesSpec {
  // Need to define here since AnyVal class cannot be local
  object `Should succeed with AnyValue`:
    case class Info(value: String) extends AnyVal
    case class ContactComplex(firstName: String, lastName: String, age: Int, addressFk: Int, extraInfo: Info)
}

class FlicerMapTypesSpec extends Spec with Inside {
  val ctx = testContext
  import ctx._

  case class Contact(firstName: String, lastName: String, age: Int, addressFk: Int, extraInfo: String)
  val contacts = List(
    Contact("Joe", "Bloggs", 123, 1, "1"),
    Contact("Joe", "Noggs", 123, 1, "1"),
    Contact("Jim", "Roggs", 111, 2, "2")
  )
  case class DateEncodingTestEntity(v1: LocalDate, v2: LocalDateTime)
  def makeEntity(i: Int) = {
    val ld = LocalDate.of(2022, i, i)
    val ldt = LocalDateTime.of(2022, i, i, i, i, i)
    DateEncodingTestEntity(ld, ldt)
  }
  val dates = List(
    makeEntity(1),
    makeEntity(2),
    makeEntity(3)
  )

  override def beforeAll(): Unit = {
    val people = quote { query[Contact] }
    ctx.run(people.delete)
    ctx.run(liftQuery(contacts).foreach(c => people.insertValue(c)))
    ctx.run(query[DateEncodingTestEntity].delete)
    ctx.run(liftQuery(dates).foreach(c => query[DateEncodingTestEntity].insertValue(c)))
  }

  "Should do correct query from map with" - {
    "simple datatypes" in {
      ctx.run(query[Contact].filterByKeys(Map("firstName" -> "Joe", "addressFk" -> 1))) mustEqual
        List(Contact("Joe","Bloggs",123,1,"1"), Contact("Joe","Noggs",123,1,"1"))
    }
    "string datatypes" in {
      ctx.run(query[Contact].filterByKeys(Map("firstName" -> "Joe", "addressFk" -> "1"))) mustEqual
        List(Contact("Joe","Bloggs",123,1,"1"), Contact("Joe","Noggs",123,1,"1"))
    }
    "date datatypes" in {
      ctx.run(query[DateEncodingTestEntity].filterByKeys(Map("v1" -> makeEntity(1).v1, "v2" -> makeEntity(1).v2))) mustEqual List(makeEntity(1))
    }
    "date-string datatypes" in {
      ctx.run(query[DateEncodingTestEntity].filterByKeys(Map("v1" -> makeEntity(1).v1.toString, "v2" -> makeEntity(1).v2.toString))) mustEqual List(makeEntity(1))
    }

  }

  "Should fail when cannot convert type from string" - {
    case class Info(value: String)
    implicit val encodeStrWrap: MappedEncoding[Info, String] = MappedEncoding[Info, String](_.value)
    implicit val decodeStrWrap: MappedEncoding[String, Info] = MappedEncoding[String, Info](Info.apply)

    case class ContactComplex(firstName: String, lastName: String, age: Int, addressFk: Int, extraInfo: Info)
    val converted = contacts.map(c => ContactComplex(c.firstName, c.lastName, c.age, c.addressFk, Info(c.extraInfo)))

    val map = Map[String, Any]("extraInfo" -> "1")
    assertThrows[IllegalStateException] {
      ctx.run(querySchema[ContactComplex]("Contact").filterByKeys(map))
    }
  }

  "Should succeed with AnyValue" in {
    import FlicerMapTypesSpec.`Should succeed with AnyValue`._
    val converted = contacts.map(c => ContactComplex(c.firstName, c.lastName, c.age, c.addressFk, Info(c.extraInfo)))

    val map = Map[String, Any]("extraInfo" -> Info("1"))
    ctx.run(querySchema[ContactComplex]("Contact").filterByKeys(map)) mustEqual
      List(ContactComplex("Joe","Bloggs",123,1,Info("1")), ContactComplex("Joe","Noggs",123,1,Info("1")))
  }
}