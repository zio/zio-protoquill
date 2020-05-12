package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuotationBin
import miniquill.quoter.Dsl._
import miniquill.quoter._
import io.getquill._
import io.getquill.ast.{Query => AQuery, _}
import miniquill.quoter.Quoted
import miniquill.quoter.ScalarPlanter
import miniquill.quoter.QuotationVase
import miniquill.quoter.QuotationBin
import org.scalatest._
import io.getquill.context.ExecutionType

case class PersonName(name: String)

class QueryMetaTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorIdiom, Literal)
  import ctx._

  
  

  "summon schema meta" - {
    implicit inline given qm: QueryMeta[PersonName, String] = {
      queryMeta[PersonName, String](
        quote { 
          (q: Query[PersonName]) => q.map(p => p.name)
        }
      )((name: String) => PersonName(name))
    }

    
    printer.lnf(qm.entity.ast)

    "static" in {
      inline def people = quote { query[PersonName] }
      val result = ctx.run(people)
      result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
      result.executionType mustEqual ExecutionType.Static
    }

    "dynamic" in {
      val people = quote { query[PersonName] }
      val result = ctx.run(people)
      result.string mustEqual """querySchema("PersonName").map(p => p.name)"""
      result.executionType mustEqual ExecutionType.Dynamic
    }
  }
}