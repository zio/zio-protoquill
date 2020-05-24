package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuotationLot
import miniquill.quoter.Dsl._
import miniquill.quoter._
import io.getquill._
import io.getquill.ast.{Query => AQuery, _}
import miniquill.quoter.Quoted
import miniquill.quoter.ScalarPlanter
import miniquill.quoter.QuotationVase
import miniquill.quoter.QuotationLot
import org.scalatest._
import io.getquill.context.ExecutionType

case class PersonName(name: String)

class QueryMetaTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorIdiom, Literal)
  import ctx._

  
  

  "summon schema meta" - {
    "static meta" - {
      implicit inline def qm: QueryMeta[PersonName, String] = {
        queryMeta[PersonName, String](
          quote { 
            (q: Query[PersonName]) => q.map(p => p.name)
          }
        )((name: String) => PersonName(name))
      }

      printer.lnf(qm.entity.ast)

      "static query" in {
        inline def people = quote { query[PersonName] }
        val result = ctx.run(people)
        result.executionType mustEqual ExecutionType.Static
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
      }

      "dynamic query" in {
        val people = quote { query[PersonName] }
        val result = ctx.run(people)
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
        result.executionType mustEqual ExecutionType.Dynamic
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
        result.executionType mustEqual ExecutionType.Dynamic
      }

      "dynamic query" in {
        val people = quote { query[PersonName] } //helloo
        val result = ctx.run(people)
        result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
        result.executionType mustEqual ExecutionType.Dynamic
      }
    }
  }
}