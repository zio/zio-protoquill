package io.getquill.context.jdbc.postgres

import io.getquill.context.sql.PeopleAggregationSpec
import io.getquill._
import io.getquill.util.debug.PrintMac.apply

class PeopleDynamicComboSpec extends Spec {
  val context: testContext.type = testContext
  import testContext._

  case class Contact(firstName: String, lastName: String, age: Int, addressFk: Int)
  case class Address(id: Int, street: String, zip: Int = 0)
  given PostgresJdbcContext.Codec.CompositeDecoder[Contact] = PostgresJdbcContext.Codec.deriveComposite
  given PostgresJdbcContext.Codec.CompositeDecoder[Address] = PostgresJdbcContext.Codec.deriveComposite



  val testPeople = List(
    Contact("Joe", "A", 20, 1),
    Contact("Joe", "A", 30, 1),
    Contact("Joe", "B", 40, 1),
    Contact("Jim", "J", 50, 1),
    Contact("Dan", "E", 60, 1)
  )
  val testAddresses = List(
    Address(1, "111 St")
  )

  override def beforeAll() = {
    testContext.transaction {
      testContext.run(query[Contact].delete)
      testContext.run(query[Address].delete)
      testContext.run(liftQuery(testPeople).foreach(p => query[Contact].insertValue(p)))
      testContext.run(liftQuery(testAddresses).foreach(a => query[Address].insertValue(a)))
    }
    ()
  }

  sealed trait Filter {
    def value: String
    def fieldName: String
  }
  object Filter {
    case class FirstName(value: String) extends Filter { val fieldName = "firstName" }
    case class LastName(value: String) extends Filter { val fieldName = "lastName" }
  }
  given PostgresJdbcContext.Codec.CompositeDecoder[Filter.FirstName] = PostgresJdbcContext.Codec.deriveComposite
  given PostgresJdbcContext.Codec.CompositeDecoder[Filter.LastName] = PostgresJdbcContext.Codec.deriveComposite

  "filter by multiple case should work" in {
    val criteria = List(Filter.FirstName("Joe"), Filter.LastName("A"))

    val q: Quoted[Query[Contact]] = quote { query[Contact] }
    val combined = {
      // The single 'lift' instance should produce a situation where the lift needs to be deduped by dedupeRuntimeBinds method
      // during dynamic query execution (in PrepareDynamicExecution.apply)
      criteria.foldLeft(q) {
          case (q, fil) => q.filter(c => sql"#${fil.fieldName}".as[String] == lift(fil.value))
      }
    }
    context.run(combined) must contain theSameElementsAs testPeople.filter(p => p.firstName == "Joe" && p.lastName == "A")
  }
}
