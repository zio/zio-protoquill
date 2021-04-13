package io.getquill.ported.quotation

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Dsl._
import io.getquill._
import io.getquill._
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.Quoted
import io.getquill.Planter
import io.getquill.QuotationVase
import io.getquill.QuotationLot
import org.scalatest._
import io.getquill.quat.Quat

class FunctionQuotationTest extends Spec with Inside {
  // val ctx = new MirrorContext(MirrorIdiom, Literal)
  // import ctx._

  case class Person(name: String, age: Int)

  "function" - {
    "anonymous function" in {
      inline def q = quote {
        (s: String) => s
      }
      quote(unquote(q)).ast mustEqual Function(List(Ident("s")), Ident("s"))
    }
    "with type parameter" in {
      inline def q[T] = quote {
        (q: Query[T]) => q
      }
      //IsDynamic(q.ast) mustEqual false
      quote(unquote(q)).ast mustEqual Function(List(Ident("q")), Ident("q"))
    }
  }
  "function apply" - {
    "local function" in {
      inline def f = quote {
        (s: String) => s
      }
      inline def q = quote {
        f("s")
      }
      quote(unquote(q)).ast mustEqual Constant("s", Quat.Value)
    }
    "function reference" in {
      inline def q = quote {
        (f: String => String) => f("a")
      }
      quote(unquote(q)).ast.asInstanceOf[Function].body mustEqual FunctionApply(Ident("f"), List(Constant("a", Quat.Value)))
      quote(unquote(q)).ast mustEqual Function(List(Ident("f")), FunctionApply(Ident("f"), List(Constant("a", Quat.Value))))
    }
  }
}