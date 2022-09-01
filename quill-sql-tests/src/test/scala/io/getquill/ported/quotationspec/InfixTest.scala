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
import io.getquill.PicklingHelper._
import io.getquill.context.ExecutionType

class InfixTest extends Spec with Inside {
  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")

  "sql" - {
    "with `as`" in {
      inline def q = quote {
        sql"true".as[Boolean]
      }
      val i = Infix(List("true"), Nil, false, false, Quat.BooleanValue)
      quote(unquote(q)).ast mustEqual i
      repickle(i) mustEqual i
    }
    "with `as` and `generic`" in {
      inline def q = quote {
        sql"true".generic.pure.as[Boolean]
      }
      val i = Infix(List("true"), Nil, true, false, Quat.Generic)
      quote(unquote(q)).ast mustEqual i
      repickle(i) mustEqual i
    }
    "with `as` and `transparent`" in {
      inline def q = quote {
        sql"true".transparent.pure.as[Boolean]
      }
      val i = Infix(List("true"), Nil, true, true, Quat.Generic)
      quote(unquote(q)).ast mustEqual i
      repickle(i) mustEqual i
    }
    "with no `as`" in {
      inline def q = quote {
        sql"true"
      }
      val i = Infix(List("true"), Nil, false, false, Quat.Value)
      quote(unquote(q)).ast mustEqual i
    }
    "with params" in {
      inline def q = quote {
        (a: String, b: String) =>
          sql"$a || $b".as[String]
      }
      val i = Infix(List("", " || ", ""), List(Ident("a"), Ident("b")), false, false, Quat.Value)
      quote(unquote(q)).ast.body mustEqual i
      repickle(i) mustEqual i
    }

    "with dynamic string" - {
      object Vase:
        def unapply(vase: QuotationVase) =
          vase match
            case QuotationVase(Quoted(ast, Nil, Nil), uid) => Some((ast, uid))
            case _ => None

      "at the end - pure" in {
        val b = "dyn"
        inline def q = quote {
          (a: String) =>
            sql"$a || #$b".pure.as[String]
        }
        q must matchPattern {
          case Quoted(
            Function(List(Ident("a", QV)), QuotationTag(idA)), Nil,
            List(Vase(Infix(List("", " || dyn"), List(Ident("a", Quat.Value)), true, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
      "at the end" in {
        val b = "dyn"
        val q = quote {
          (a: String) =>
            sql"$a || #$b".as[String]
        }
        q must matchPattern {
          case Quoted(
            Function(List(Ident("a", QV)), QuotationTag(idA)), Nil,
            List(Vase(Infix(List("", " || dyn"), List(Ident("a", Quat.Value)), false, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
      "at the beginning - pure" in {
        val a = "dyn"
        val q = quote {
          (b: String) =>
            sql"#$a || $b".pure.as[String]
        }
        q must matchPattern {
          case Quoted(
            Function(List(Ident("b", QV)), QuotationTag(idA)), Nil,
            List(Vase(Infix(List("dyn || ", ""), List(Ident("b", Quat.Value)), true, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
      "at the beginning" in {
        val a = "dyn"
        val q = quote {
          (b: String) =>
            sql"#$a || $b".as[String]
        }
        q must matchPattern {
          case Quoted(
            Function(List(Ident("b", QV)), QuotationTag(idA)), Nil,
            List(Vase(Infix(List("dyn || ", ""), List(Ident("b", Quat.Value)), false, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
      "only" in {
        val a = "dyn1"
        val q = quote {
          sql"#$a".as[String]
        }

        q must matchPattern {
          case Quoted(
            QuotationTag(idA), Nil,
            List(Vase(Infix(List("dyn1"), Nil, false, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
      "with lift" in {
        import testContext._
        val a = "dyn1"
        val q = quote {
          sql"#$a || ${lift("foo")}".as[String]
        }

        q must matchPattern {
          case Quoted(
            QuotationTag(idA),
            List(EagerPlanter("foo", _, idB)),
            List(Vase(Infix(List("dyn1 || ", ""), List(ScalarTag(idB1, _)), false, false, QV), idA1))
          ) if (idA == idA1 && idB == idB1) =>
        }
      }
      "sequential - pure" in {
        val a = "dyn1"
        val b = "dyn2"
        val q = quote {
          sql"#$a#$b".pure.as[String]
        }
        q must matchPattern {
          case Quoted(
            QuotationTag(idA), Nil,
            List(Vase(Infix(List("dyn1dyn2"), Nil, true, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
      "sequential" in {
        val a = "dyn1"
        val b = "dyn2"
        val q = quote {
          sql"#$a#$b".as[String]
        }
        q must matchPattern {
          case Quoted(
            QuotationTag(idA), Nil,
            List(Vase(Infix(List("dyn1dyn2"), Nil, false, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
      "non-string value" in {
        case class Value(a: String)
        val a = Value("dyn")
        val q = quote {
          sql"#$a".as[String]
        }
        q must matchPattern {
          case Quoted(
            QuotationTag(idA), Nil,
            List(Vase(Infix(List("Value(dyn)"), Nil, false, false, QV), idA1))
          ) if (idA == idA1) =>
        }
      }
    }

    "in a context" - {
      val ctx = new SqlMirrorContext(PostgresDialect, Literal)
      import ctx._
      case class Person(name: String, age: Int)
      "dynamic with property" in {
        val fun = "DYNAMIC_FUNC"
        ctx.run(query[Person].map(p => sql"#$fun(${p.name})".as[String])).triple mustEqual
          ("SELECT DYNAMIC_FUNC(p.name) FROM Person p", List(), ExecutionType.Dynamic)
      }

      "dynamic with property and lift" in {
        val liftVar = "LIFT_VAR"
        val fun = "DYNAMIC_FUNC"
        ctx.run(query[Person].map(p => sql"#$fun(${p.name}, ${lift(liftVar)})".as[String])).triple mustEqual
          ("SELECT DYNAMIC_FUNC(p.name, ?) FROM Person p", List("LIFT_VAR"), ExecutionType.Dynamic)
      }
    }
  }
}