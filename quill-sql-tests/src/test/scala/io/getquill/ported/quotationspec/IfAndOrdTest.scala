package io.getquill.ported.quotationspec

import org.scalatest._
import io.getquill._
import io.getquill.ast._
import io.getquill.PicklingHelper._

class IfAndOrdTest extends Spec with TestEntities with Inside {

  extension (ast: Ast) {
    def ordering: Ast = ast match {
      case f: SortBy => f.ordering
      case _ => fail("Not a sortby, can't get ordering")
    }
    def body: Ast = ast match {
      case f: Function => f.body
      case _ => fail(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")
    }
  }

  "if" - {
    "simple" in {
      inline def q = quote {
        (c: Boolean) => if (c) 1 else 2
      }
      val f = If(Ident("c"), Constant.auto(1), Constant.auto(2))
      quote(unquote(q)).ast.body mustEqual f
      repickle(f) mustEqual f
    }
    "nested" in {
      inline def q = quote {
        (c1: Boolean, c2: Boolean) => if (c1) 1 else if (c2) 2 else 3
      }
      val f = If(Ident("c1"), Constant.auto(1), If(Ident("c2"), Constant.auto(2), Constant.auto(3)))
      quote(unquote(q)).ast.body mustEqual f
      repickle(f) mustEqual f
    }
  }
  "ord" in {
    inline def o = quote {
      Ord.desc[Int]
    }
    inline def q = quote {
      qr1.sortBy(_.i)(o)
    }
    quote(unquote(q)).ast.ordering mustEqual o.ast
    repickle(q.ast) mustEqual q.ast
  }
}