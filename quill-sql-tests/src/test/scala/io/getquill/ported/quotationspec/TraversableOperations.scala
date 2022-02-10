package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.Quoted
import io.getquill.Planter
import io.getquill.QuotationVase
import io.getquill.QuotationLot
import org.scalatest._
import io.getquill.quat.Quat
import io.getquill.ast.Implicits._
import io.getquill.norm.NormalizeStringConcat
import io.getquill._

class TraversableOperations extends Spec with TestEntities with Inside {

  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")

  "traversable operations" - {
    "map.contains" in {
      inline def q = quote {
        (m: collection.Map[Int, String], k: Int) => m.contains(k)
      }
      quote(unquote(q)).ast.body mustEqual MapContains(Ident("m"), Ident("k"))
    }
    "set.contains" in {
      inline def q = quote {
        (s: Set[Int], v: Int) => s.contains(v)
      }
      quote(unquote(q)).ast.body mustEqual SetContains(Ident("s"), Ident("v"))
    }
    "list.contains" in {
      inline def q = quote {
        (l: List[Int], v: Int) => l.contains(v)
      }
      quote(unquote(q)).ast.body mustEqual ListContains(Ident("l"), Ident("v"))
    }
  }
}
