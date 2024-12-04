package io.getquill.context.jdbc.postgres

import io.getquill.context.encoding.OptionalNestedSpec
import io.getquill._

class OptionalProductEncodingJdbcSpec extends OptionalNestedSpec with PostgresJdbcContext.Codec {

  val context: testContext.type = testContext
  import testContext._

  given setupContactDecoder: PostgresJdbcContext.Codec.CompositeDecoder[Setup.Contact] = PostgresJdbcContext.Codec.deriveComposite
  given ex1LastNameAgeDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`1.Optional Inner Product`.LastNameAge] = PostgresJdbcContext.Codec.deriveComposite
  given ex1ContactDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`1.Optional Inner Product`.Contact] = PostgresJdbcContext.Codec.deriveComposite
  given ex2AgeDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`2.Optional Inner Product with Optional Leaf`.Age] = PostgresJdbcContext.Codec.deriveComposite
  given ex2LastNameAgeDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`2.Optional Inner Product with Optional Leaf`.LastNameAge] = PostgresJdbcContext.Codec.deriveComposite
  given ex2ContactDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`2.Optional Inner Product with Optional Leaf`.Contact] = PostgresJdbcContext.Codec.deriveComposite

  override protected def beforeEach() = {
    import Setup._
    testContext.run(query[Contact].delete)
    ()
  }

  "1.Optional Inner Product" - {
    import `1.Optional Inner Product`._
    "1.Ex1 - Not null inner product" in {
      context.run(`1.Ex1 - Not null inner product insert`)
      context.run(data) mustEqual List(`1.Ex1 - Not null inner product result`)
    }
    "1.Ex1 Auto - Not null inner product" in {
      inline def result = `1.Ex1 - Not null inner product result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }

    "1.Ex2 - null inner product" in {
      context.run(`1.Ex2 - null inner product insert`)
      context.run(data) mustEqual List(`1.Ex2 - null inner product result`)
    }
    "1.Ex2 Auto - null inner product" in {
      inline def result = `1.Ex2 - null inner product result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }
  }

  "2.Optional Inner Product" - {
    import `2.Optional Inner Product with Optional Leaf`._
    "2.Ex1 - Not null inner product" in {
      context.run(`2.Ex1 - not-null insert`)
      context.run(data) mustEqual List(`2.Ex1 - not-null result`)
    }
    "2.Ex1 Auto - Not null inner product" in {
      inline def result = `2.Ex1 - not-null result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }

    "2.Ex2 - Not null inner product" in {
      context.run(`2.Ex2 - Null inner product insert`)
      context.run(data) mustEqual List(`2.Ex2 - Null inner product result`)
    }
    "2.Ex2 Auto - Not null inner product" in {
      inline def result = `2.Ex2 - Null inner product result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }

    "2.Ex3 - Null inner leaf" in {
      context.run(`2.Ex3 - Null inner leaf insert`)
      context.run(data) mustEqual List(`2.Ex3 - Null inner leaf result`)
    }
    "2.Ex3 Auto - Null inner leaf" in {
      inline def result = `2.Ex3 - Null inner leaf result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }
  }

  "3.Optional Nested Inner Product" - {
    import `3.Optional Nested Inner Product`._
    given ex3AgeDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`3.Optional Nested Inner Product`.Age] = PostgresJdbcContext.Codec.deriveComposite
    given ex3LastNameAgeDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`3.Optional Nested Inner Product`.LastNameAge] = PostgresJdbcContext.Codec.deriveComposite
    given ex3ContactDecoder: PostgresJdbcContext.Codec.CompositeDecoder[`3.Optional Nested Inner Product`.Contact] = PostgresJdbcContext.Codec.deriveComposite

    "3.Ex1 - Null inner product insert" in {
      context.run(`3.Ex1 - Null inner product insert`)
      context.run(data) mustEqual List(`3.Ex1 - Null inner product result`)
    }
    "3.Ex1 Auto - Null inner product insert" in {
      inline def result = `3.Ex1 - Null inner product result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }

    "3.Ex2 - Null inner leaf" in {
      context.run(`3.Ex2 - Null inner leaf insert`)
      context.run(data) mustEqual List(`3.Ex2 - Null inner leaf result`)
    }
    "3.Ex2 Auto - Null inner leaf" in {
      inline def result = `3.Ex2 - Null inner leaf result`
      context.run(data.insertValue(lift(result)))
      context.run(data) mustEqual List(result)
    }
  }
}
