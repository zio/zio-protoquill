package io.getquill.context.jdbc.sqlite

import io.getquill.context.sql.PeopleReturningSpec
import io.getquill.*
import io.getquill.context.jdbc.JdbcVerbs


class PeopleJdbcReturningSpecEncoders extends PeopleReturningSpec with JdbcVerbs {

  val context = testContext
  import testContext._

  override def beforeEach(): Unit = {
    testContext.transaction {
      testContext.run(query[Contact].delete)
      testContext.run(query[Product].delete)
      testContext.run(liftQuery(people).foreach(p => peopleInsert(p)))
    }
    super.beforeEach()
  }

  "Ex 0 insert.returning(_.generatedColumn) mod" in {
    import `Ex 0 insert.returning(_.generatedColumn) mod`._
    val id = testContext.run(op)
    testContext.run(get).toSet mustEqual result(id).toSet
  }

  // multiple return columns not allowed in SQLite
  "Ex 0.5 insert.returning(wholeRecord) mod" ignore {
    import `Ex 0.5 insert.returning(wholeRecord) mod`._
    "testContext.run(op)" mustNot compile
  }

  "Ex 1 insert.returningMany(_.generatedColumn) mod" in {
    import `Ex 1 insert.returningMany(_.generatedColumn) mod`._
    val id = testContext.run(op)
    testContext.run(get) mustEqual result(id.head)
  }

  // Not supported in SQLite
  "Ex 2 update.returningMany(_.singleColumn) mod" ignore {
    import `Ex 2 update.returningMany(_.singleColumn) mod`._
    testContext.run(op) must contain theSameElementsAs expect
    testContext.run(get) must contain theSameElementsAs result
  }

  // Not supported in SQLite
  "Ex 3 delete.returningMany(wholeRecord)" in {
    import `Ex 3 delete.returningMany(wholeRecord)`._
    "testContext.run(op)" mustNot compile
  }

  // Not supported in SQLite
  "Ex 4 update.returningMany(query)" ignore {
    import `Ex 4 update.returningMany(query)`._
    testContext.run(op) must contain theSameElementsAs expect
    testContext.run(get) must contain theSameElementsAs result
  }
}
