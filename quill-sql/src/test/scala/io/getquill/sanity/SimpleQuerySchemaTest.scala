package io.getquill.sanity

import io.getquill._
import io.getquill.generic._
import io.getquill.context.SplicingBehaviorHint
import io.getquill.context.SplicingBehavior

class SimpleQuerySchemaTest extends Spec {

  given SplicingBehaviorHint with
    override type BehaviorType = SplicingBehavior.FailOnDynamic

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  case class Person(name: String, age: Int)

  "query schema should" - {
    "produce an sql query with a renamed table" in {
      inline def q = quote {
        querySchema[Person]("tblPerson")
      }
      ctx.run(q).string mustEqual "SELECT x.name, x.age FROM tblPerson x"
    }
    "produce an sql query with a renamed table and renamed columns" in {
      inline def q = quote {
        querySchema[Person]("tblPerson", _.name -> "colName")
      }
      ctx.run(q).string mustEqual "SELECT x.colName, x.age FROM tblPerson x"
    }
    "produce an sql query with a filter and a renamed table and renamed columns" in {
      inline def q = quote {
        querySchema[Person]("tblPerson", _.name -> "colName").filter(p => p.name == "Joe")
      }
      ctx.run(q).string mustEqual "SELECT p.colName, p.age FROM tblPerson p WHERE p.colName = 'Joe'"
    }
  }
  "schemaMeta should" - {
    "produce an sql query with a filter and a renamed table and renamed columns" in {
      inline given SchemaMeta[Person] = schemaMeta("tblPerson", _.name -> "colName")
      inline def q = quote {
        query[Person].filter(p => p.name == "Joe")
      }
      ctx.run(q).string mustEqual "SELECT p.colName, p.age FROM tblPerson p WHERE p.colName = 'Joe'"
    }
  }
}
