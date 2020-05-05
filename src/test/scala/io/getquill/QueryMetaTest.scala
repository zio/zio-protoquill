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

class QueryMetaTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorIdiom, Literal)
  import ctx._

  
  
  

  "summon schema meta" in {
    implicit inline given qm: QueryMeta[PersonName, String] = {
      queryMeta[PersonName, String](
        quote { 
          (q: Query[PersonName]) => q.map(p => p.name)
        }
      )((name: String) => PersonName(name))
    }

    println("~~~~~~~~~~~~~~~~ Query Schema Entity ~~~~~~~~~~~~~")
    printer.lnf(qm.entity.ast)

    inline def people = quote { query[PersonName] } //helloooooooooooooo
    println("************************* Output ********************")
    println( ctx.run(people) )
  }
}