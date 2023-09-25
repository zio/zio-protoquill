package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import org.scalatest._
import io.getquill.quat.Quat
import io.getquill._
import io.getquill.PicklingHelper._

class FunctionQuotationTest extends Spec with Inside {
  case class Person(name: String, age: Int)

  "function" - {
    "anonymous function" in {
      inline def q = quote { (s: String) =>
        s
      }
      val f = Function(List(Ident("s")), Ident("s"))
      quote(unquote(q)).ast mustEqual f
      repickle(f) mustEqual f
    }
    "with type parameter" in {
      inline def q[T] = quote { (q: Query[T]) =>
        q
      }
      // IsDynamic(q.ast) mustEqual false
      val f = Function(List(Ident("q")), Ident("q"))
      quote(unquote(q)).ast mustEqual f
      repickle(f) mustEqual f
    }
  }
  "function apply" - {
    "local function" in {
      inline def f = quote { (s: String) =>
        s
      }
      inline def q = quote {
        f("s")
      }
      val c = Constant("s", Quat.Value)
      quote(unquote(q)).ast mustEqual c
      repickle(c) mustEqual c
    }
    "function reference" in {
      inline def q = quote { (f: String => String) =>
        f("a")
      }
      val ff = Function(List(Ident("f")), FunctionApply(Ident("f"), List(Constant("a", Quat.Value))))
      quote(unquote(q)).ast mustEqual ff
      repickle(ff) mustEqual ff
    }
  }
}
