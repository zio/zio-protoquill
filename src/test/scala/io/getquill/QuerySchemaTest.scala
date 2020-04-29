package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._
import miniquill.quoter.QuoteDsl
import miniquill.quoter.Quoted
import miniquill.quoter._
import io.getquill._
import io.getquill.ast._
import miniquill.quoter.QuotationBin
import miniquill.quoter.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._

class QuerySchemaTest extends Spec with Inside { //hellooooooo

  "quoting schema meta" - {
    case class OneLevelPerson(name: String, age: Int)
    case class OneLevelPersonOpt(name: String, age: Option[Int])
    case class Age(value: Int)
    case class TwoLevelPerson(name: String, age: Age)
    case class TwoLevelPersonOpt(name: String, age: Option[Int])

    "should parse one-level" in { //hellooooo
      def schema = QuoteDsl.schemaMeta[OneLevelPerson]("personTbl", _.name -> "nameCol", _.age -> "ageCol")
      val ent = Entity("personTbl", List(PropertyAlias(List("name"), "nameCol"), PropertyAlias(List("age"), "ageCol")))
      schema.entity.ast mustEqual ent
    }
  }

  // "schema meta lookup" - {
  //   case class Person(name: String, age: Int)
  //   inline given SchemaMeta[Person] = schemaMeta[Person]("tblPerson")
  //   val q = quote { query[Person].map(p => p.name) }
  //   printer.lnf(q.ast)
  // }
  
}