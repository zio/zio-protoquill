package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.QuotationLot
import io.getquill.quoter.Dsl._
import io.getquill.quoter._
import io.getquill._
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quoter.Quoted
import io.getquill.quoter.ScalarPlanter
import io.getquill.quoter.QuotationVase
import io.getquill.quoter.QuotationLot
import org.scalatest._

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
        quote(unquote(q)).ast mustEqual Constant("s")
      }
      "function reference" in {
        inline def q = quote {
          (f: String => String) => f("a")
        }
        quote(unquote(q)).ast.asInstanceOf[Function].body mustEqual FunctionApply(Ident("f"), List(Constant("a")))
        quote(unquote(q)).ast mustEqual Function(List(Ident("f")), FunctionApply(Ident("f"), List(Constant("a"))))
      }
    }
}