package io.getquill

import scala.language.implicitConversions
import io.getquill.QuotationLot

import io.getquill.ast.{Query => AQuery, _}
import io.getquill.Quoted
import io.getquill.Planter
import io.getquill.QuotationVase
import io.getquill.QuotationLot
import org.scalatest._
import io.getquill.context.ExecutionType

case class PersonName(name: String)

class QueryMetaTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorIdiom, Literal)
  import ctx._

  "summon schema meta" - {
    "static meta - new style" - {
      inline given QueryMeta[PersonName, String] =
        queryMeta(
          quote {
            (q: Query[PersonName]) => q.map(p => p.name)
          }
        )((name: String) => PersonName(name))

      "static query" in {
        inline def people = quote { query[PersonName] }
        val result = ctx.run(people)
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
        result.info.executionType mustEqual ExecutionType.Static
      }
    }

    "static meta - new style - multi object" - {
      case class Person(name: String, age: Int, importance: String)
      case class Vip(name: String, age: Int)

      inline given QueryMeta[Vip, Person] =
        queryMeta(
          quote {
            (q: Query[Vip]) => q.map(v => Person(v.name, v.age, "VeryImportant"))
          }
        )((p: Person) => Vip(p.name, p.age))

      "static query" in {
        inline def people = quote { query[Vip] }
        val result = ctx.run(people)
        result.string mustEqual """querySchema("Vip").map(v => CaseClass(name: v.name, age: v.age, importance: "VeryImportant"))"""
        result.info.executionType mustEqual ExecutionType.Static
      }

      "dynamic query" in {
        val people = quote { query[Vip] }
        val result = ctx.run(people)
        result.string mustEqual """querySchema("Vip").map(v => CaseClass(name: v.name, age: v.age, importance: "VeryImportant"))"""
        result.info.executionType mustEqual ExecutionType.Dynamic
      }
    }

    "static meta" - {
      implicit inline def qm: QueryMeta[PersonName, String] = { // hellooo
        queryMeta[PersonName, String](
          quote {
            (q: Query[PersonName]) => q.map(p => p.name)
          }
        )((name: String) => PersonName(name))
      }

      "static query should yield static result" in {
        inline def people = quote { query[PersonName] }
        val result = ctx.run(people)
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
        result.info.executionType mustEqual ExecutionType.Static
      }

      "dynamic query shuold yield dynamic request" in {
        val people = quote { query[PersonName] }
        val result = ctx.run(people)
        // println("Result: " + result.string)
        // println("=== Result: " + io.getquill.util.Messages.qprint(result.info.ast))
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
        result.info.executionType mustEqual ExecutionType.Dynamic
      }
    }

    "dynamic meta" - {
      implicit val qm: QueryMeta[PersonName, String] = {
        queryMeta[PersonName, String](
          quote {
            (q: Query[PersonName]) => q.map(p => p.name)
          }
        )((name: String) => PersonName(name))
      }

      "static query" in {
        inline def people: Quoted[Query[PersonName]] = quote { query[PersonName] }
        val result = ctx.run[PersonName](people)
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
        result.info.executionType mustEqual ExecutionType.Dynamic
      }

      "dynamic query" in {
        val people = quote { query[PersonName] }
        val result = ctx.run(people)
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
        result.info.executionType mustEqual ExecutionType.Dynamic
      }
    }
  }
}
