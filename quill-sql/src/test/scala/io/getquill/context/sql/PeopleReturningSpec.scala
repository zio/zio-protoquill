package io.getquill.context.sql

import io.getquill._
import org.scalatest.BeforeAndAfterEach
import io.getquill.generic.{DecodingType, GenericDecoder}

trait PeopleReturningSpec extends Spec with BeforeAndAfterEach { self =>
  val context: SqlContext[_, _] {
    type Session = self.Session
    type PrepareRow = self.PrepareRow
    type ResultRow = self.ResultRow
  }
  import context._

  case class Contact(firstName: String, lastName: String, age: Int, addressFk: Int = 0, extraInfo: Option[String] = None)
  case class Product(id: Long, description: String, sku: Int)
  given contactDecoder: GenericDecoder[ResultRow, Session, Contact, DecodingType.Composite] = deriveComposite
  given productDecoder: GenericDecoder[ResultRow, Session, Product, DecodingType.Composite] = deriveComposite

  inline def peopleInsert =
    quote((p: Contact) => query[Contact].insertValue(p))

  val people = List(
    Contact("Joe", "A", 44),
    Contact("Dan", "D", 55),
    Contact("Joe", "B", 66),
    Contact("Jim", "J", 77)
  )

  val product = Product(0, "Something", 123)

  object `Ex 0 insert.returning(_.generatedColumn) mod` {
    inline def op = quote {
      query[Product].insert(_.description -> lift(product.description), _.sku -> lift(product.sku)).returning(p => p.id)
    }
    inline def get = quote { query[Product] }
    def result(id: Long) = List(product.copy(id = id))
  }

  object `Ex 0.5 insert.returning(wholeRecord) mod` {
    inline def op = quote {
      query[Product].insert(_.description -> lift(product.description), _.sku -> lift(product.sku)).returning(p => p)
    }
    inline def get = quote { query[Product] }
    def result(newProduct: Product) = List(newProduct)
  }

  object `Ex 1 insert.returningMany(_.generatedColumn) mod` {
    inline def op = quote {
      query[Product].insert(_.description -> lift(product.description), _.sku -> lift(product.sku)).returningMany(p => p.id)
    }
    inline def get = quote { query[Product] }
    def result(id: Long) = List(product.copy(id = id))
  }

  object `Ex 2 update.returningMany(_.singleColumn) mod` {
    inline def op = quote {
      query[Contact].filter(p => p.firstName == "Joe").update(p => p.age -> (p.age + 1)).returningMany(p => p.lastName)
    }
    val expect = people.filter(_.firstName == "Joe").map(_.lastName)
    inline def get = quote { query[Contact] }
    inline def result = people.map(p => if (p.firstName == "Joe") p.copy(age = p.age + 1) else p)
  }

  object `Ex 3 delete.returningMany(wholeRecord)` {
    inline def op = quote {
      query[Contact].filter(p => p.firstName == "Joe").delete.returningMany(p => p)
    }
    val expect = people.filter(p => p.firstName == "Joe")
    inline def get = quote { query[Contact] }
    inline def result = people.filterNot(p => p.firstName == "Joe")
  }

  object `Ex 4 update.returningMany(query)` {
    inline def op = quote {
      query[Contact]
        .filter(p => p.firstName == "Joe")
        .update(p => p.age -> (p.age + 1))
        .returningMany(p => query[Contact].filter(cp => cp.firstName == p.firstName && cp.lastName == p.lastName).map(_.lastName).value.orNull)
    }
    val expect = List("A", "B")
    inline def get = quote { query[Contact] }
    inline def result = people.map(p => if (p.firstName == "Joe") p.copy(age = p.age + 1) else p)
  }
}
