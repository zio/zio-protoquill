package io.getquill

import io.getquill._
import io.getquill.dsl._
import io.getquill.quoter.Dsl._

class QuerySchemaExampleTest extends Spec {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  case class Person(name: String, age:Int)

  "query schema shuold" - {
    "produce an sql query with a renamed table" in {
      val q = quote {
        querySchema[Person]("tblPerson")
      }
      ctx.run(q).string mustEqual "SELECT x.name, x.age FROM tblPerson x"
    }
    "produce an sql query with a renamed table and renamed columns" in {
      val q = quote {
        querySchema[Person]("tblPerson", _.name -> "colName")
      }
      ctx.run(q).string mustEqual "SELECT x.colName, x.age FROM tblPerson x"
    }
  }

  "query should" - {
    "produce a 'where' clause from a filter" in {
      val q = quote {
        query[Person].filter(p => p.name == "Joe")
      }
      println(ctx.run(q).string) ///mustEqual "SELECT p.name, p.age FROM Person WHERE p.name == 'Joe'"
    }
  }

  // "some logic tests" - {
  //   "test that some logic" - {
  //     "produces expected output" in {
  //       val output = doSomeLogic()
  //       assertEquals(output, "expected output")  
  //     }
  //     "produces some other expected output" in {
  //       val output = doSomeLogic()
  //       assertEquals(output, "some other expected output")  
  //     }
  //   }
  // }
}


