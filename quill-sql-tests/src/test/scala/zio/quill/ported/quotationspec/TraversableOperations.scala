package zio.quill.ported.quotationspec

import scala.language.implicitConversions
import zio.quill.QuotationLot
import zio.quill.ast.{Query => AQuery, _}
import zio.quill.Quoted
import zio.quill.Planter
import zio.quill.QuotationVase
import zio.quill.QuotationLot
import org.scalatest._
import zio.quill.quat.Quat
import zio.quill.ast.Implicits._
import zio.quill.norm.NormalizeStringConcat
import zio.quill._
import zio.quill.PicklingHelper._

class TraversableOperations extends Spec with TestEntities with Inside {

  extension (ast: Ast) {
    def body: Ast = ast match {
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${zio.quill.util.Messages.qprint(ast)}")
    }
  }

  "traversable operations" - {
    "map.contains" in {
      inline def q = quote {
        (m: collection.Map[Int, String], k: Int) => m.contains(k)
      }
      val op = MapContains(Ident("m"), Ident("k"))
      quote(unquote(q)).ast.body mustEqual op
      repickle(op) mustEqual op
    }
    "set.contains" in {
      inline def q = quote {
        (s: Set[Int], v: Int) => s.contains(v)
      }
      val op = SetContains(Ident("s"), Ident("v"))
      quote(unquote(q)).ast.body mustEqual op
      repickle(op) mustEqual op
    }
    "list.contains" in {
      inline def q = quote {
        (l: List[Int], v: Int) => l.contains(v)
      }
      val op = ListContains(Ident("l"), Ident("v"))
      quote(unquote(q)).ast.body mustEqual op
      repickle(op) mustEqual op
    }
  }
}
