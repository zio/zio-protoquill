package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quat.Quat
import io.getquill._

class FunctionTest extends Spec with NonSerializingQuotation with TestEntities {
  object IsDynamic {
    def apply(a: Ast) =
      CollectAst(a) { case d: Dynamic => d }.nonEmpty
  }

  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")

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
      IsDynamic(q.ast) mustEqual false
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
      quote(unquote(q)).ast mustEqual Constant.auto("s")
    }
    "function reference" in {
      inline def q = quote {
        (f: String => String) => f("a")
      }
      quote(unquote(q)).ast.body mustEqual FunctionApply(Ident("f"), List(Constant.auto("a")))
    }
  }
}
