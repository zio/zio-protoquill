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

class InfixTest extends Spec with Inside {
  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")

  "infix" - {
    "with `as`" in {
      inline def q = quote {
        infix"true".as[Boolean]
      }
      quote(unquote(q)).ast mustEqual Infix(List("true"), Nil, false, false, Quat.BooleanValue)
    }
    "with `as` and `generic`" in {
      inline def q = quote {
        infix"true".generic.pure.as[Boolean]
      }
      quote(unquote(q)).ast mustEqual Infix(List("true"), Nil, true, false, Quat.Generic)
    }
    "with `as` and `transparent`" in {
      inline def q = quote {
        infix"true".transparent.pure.as[Boolean]
      }
      quote(unquote(q)).ast mustEqual Infix(List("true"), Nil, true, true, Quat.Generic)
    }
    "with no `as`" in {
      inline def q = quote {
        infix"true"
      }
      quote(unquote(q)).ast mustEqual Infix(List("true"), Nil, false, false, Quat.Value)
    }
    "with params" in {
      inline def q = quote {
        (a: String, b: String) =>
          infix"$a || $b".as[String]
      }
      quote(unquote(q)).ast.body mustEqual Infix(List("", " || ", ""), List(Ident("a"), Ident("b")), false, false, Quat.Value)
    }
    // Dynamic infix not supported yet
    // "with dynamic string" - {
    //   "at the end - pure" in {
    //     val b = "dyn"
    //     val q = quote {
    //       (a: String) =>
    //         infix"$a || #$b".pure.as[String]
    //     }
    //     quote(unquote(q)).ast must matchPattern {
    //       case Function(_, Infix(List("", " || dyn"), List(Ident("a", Quat.Value)), true, QV)) =>
    //     }
    //   }
    //   "at the end" in {
    //     val b = "dyn"
    //     val q = quote {
    //       (a: String) =>
    //         infix"$a || #$b".as[String]
    //     }
    //     quote(unquote(q)).ast must matchPattern {
    //       case Function(_, Infix(List("", " || dyn"), List(Ident("a", Quat.Value)), false, QV)) =>
    //     }
    //   }
    //   "at the beginning - pure" in {
    //     val a = "dyn"
    //     val q = quote {
    //       (b: String) =>
    //         infix"#$a || $b".pure.as[String]
    //     }
    //     quote(unquote(q)).ast must matchPattern {
    //       case Function(_, Infix(List("dyn || ", ""), List(Ident("b", Quat.Value)), true, QV)) =>
    //     }
    //   }
    //   "at the beginning" in {
    //     val a = "dyn"
    //     val q = quote {
    //       (b: String) =>
    //         infix"#$a || $b".as[String]
    //     }
    //     quote(unquote(q)).ast must matchPattern {
    //       case Function(_, Infix(List("dyn || ", ""), List(Ident("b", Quat.Value)), false, QV)) =>
    //     }
    //   }
    //   "only" in {
    //     val a = "dyn1"
    //     val q = quote {
    //       infix"#$a".as[String]
    //     }
    //     quote(unquote(q)).ast mustEqual Infix(List("dyn1"), List(), false, QV)
    //   }
    //   "sequential - pure" in {
    //     val a = "dyn1"
    //     val b = "dyn2"
    //     val q = quote {
    //       infix"#$a#$b".pure.as[String]
    //     }
    //     quote(unquote(q)).ast mustEqual Infix(List("dyn1dyn2"), List(), true, QV)
    //   }
    //   "sequential" in {
    //     val a = "dyn1"
    //     val b = "dyn2"
    //     val q = quote {
    //       infix"#$a#$b".as[String]
    //     }
    //     quote(unquote(q)).ast mustEqual Infix(List("dyn1dyn2"), List(), false, QV)
    //   }
    //   "non-string value" in {
    //     case class Value(a: String)
    //     val a = Value("dyn")
    //     val q = quote {
    //       infix"#$a".as[String]
    //     }
    //     quote(unquote(q)).ast mustEqual Infix(List("Value(dyn)"), List(), false, QV)
    //   }
    // }
  }
}