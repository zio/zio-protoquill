package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quat.Quat
import io.getquill._

class ValueTest extends Spec with TestEntities {
  "value" - { //helloo
    "null" in {
      inline def q = quote("s" != null)
      quote(unquote(q)).ast.asInstanceOf[BinaryOperation].b mustEqual NullValue
    }
    "constant" in {
      inline def q = quote(11L)
      quote(unquote(q)).ast mustEqual Constant.auto(11L)
    }
    "tuple" - {
      "literal" in {
        inline def q = quote((1, "a"))
        quote(unquote(q)).ast mustEqual Tuple(List(Constant.auto(1), Constant.auto("a")))
      }
      // "arrow assoc" - {
      //   // TODO Is this even allowed in dotty?
      //   // "unicode arrow" in {
      //   //   inline def q = quote(1 → "a")
      //   //   quote(unquote(q)).ast mustEqual Tuple(List(Constant.auto(1), Constant.auto("a")))
      //   // }
      "normal arrow" in {
        inline def q = quote(1 -> "a" -> "b")
        quote(unquote(q)).ast mustEqual Tuple(List(Tuple(List(Constant.auto(1), Constant.auto("a"))), Constant.auto("b")))
      }
      "explicit `Predef.ArrowAssoc`" in {
        inline def q = quote(Predef.ArrowAssoc("a").->[String]("b"))
        quote(unquote(q)).ast mustEqual Tuple(List(Constant.auto("a"), Constant.auto("b")))
      }
      // }
    }
  }
}
