package zio.quill.ported.quotationspec

import scala.language.implicitConversions
import zio.quill.QuotationLot
import zio.quill.Spec
import zio.quill.ast.{Query => AQuery, _}
import zio.quill.quat.Quat
import zio.quill._
import zio.quill.PicklingHelper._

class FunctionTest extends Spec with TestEntities {
  object IsDynamic {
    def apply(a: Ast) =
      CollectAst(a) { case d: Dynamic => d }.nonEmpty
  }

  extension (ast: Ast) {
    def body: Ast = ast match {
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${zio.quill.util.Messages.qprint(ast)}")
    }
  }

  "function" - {
    "anonymous function" in {
      inline def q = quote {
        (s: String) => s
      }
      val f = Function(List(Ident("s")), Ident("s"))
      quote(unquote(q)).ast mustEqual f
      repickle(f) mustEqual f
    }
    "with type parameter" in {
      inline def q[T] = quote {
        (q: Query[T]) => q
      }
      // IsDynamic(q.ast) mustEqual false
      val f = Function(List(Ident("q")), Ident("q"))
      quote(unquote(q)).ast mustEqual f
      repickle(f) mustEqual f
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
      val c = Constant.auto("s")
      quote(unquote(q)).ast mustEqual c
      repickle(c) mustEqual c
    }
    "function reference" in {
      inline def q = quote {
        (f: String => String) => f("a")
      }
      val f = FunctionApply(Ident("f"), List(Constant.auto("a")))
      quote(unquote(q)).ast.body mustEqual f
      repickle(f) mustEqual f
    }
  }
}
