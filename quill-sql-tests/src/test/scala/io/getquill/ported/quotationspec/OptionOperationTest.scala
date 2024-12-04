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
import io.getquill.ast
import io.getquill.PicklingHelper._

class OptionOperationTest extends MirrorSpec with Inside {
  "option operation" - {
    import io.getquill.ast.Implicits._

    case class Row(id: Int, value: String)

    "map" - {
      "simple" in {
        inline def q = quote {
          (o: Option[Int]) => o.map(v => v)
        }
        val o = OptionMap(Ident("o"), Ident("v"), Ident("v"))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
      "unchecked" in {
        inline def q = quote {
          (o: Option[Row]) => o.map(v => v)
        }
        val o = OptionTableMap(Ident("o"), Ident("v"), Ident("v"))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
    }
    "flatMap" - {
      "simple" in {
        inline def q = quote {
          (o: Option[Int]) => o.flatMap(v => Option(v))
        }
        val o = OptionFlatMap(Ident("o"), Ident("v"), OptionApply(Ident("v")))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
      "unchecked" in {
        inline def q = quote {
          (o: Option[Row]) => o.flatMap(v => Option(v))
        }
        val o = OptionTableFlatMap(Ident("o"), Ident("v"), OptionApply(Ident("v")))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
    }
    "getOrElse" in {
      inline def q = quote {
        (o: Option[Int]) => o.getOrElse(11)
      }
      val o = OptionGetOrElse(Ident("o"), Constant.auto(11))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "map + getOrElse" in {
      inline def q = quote {
        (o: Option[Int]) => o.map(i => i < 10).getOrElse(true)
      }
      val o =
        OptionGetOrElse(
          OptionMap(Ident("o"), Ident("i"), BinaryOperation(Ident("i"), NumericOperator.`<`, Constant.auto(10))),
          Constant.auto(true)
        )
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "flatten" in {
      inline def q = quote {
        (o: Option[Option[Int]]) => o.flatten
      }
      val o = OptionFlatten(Ident("o"))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "Some" in {
      inline def q = quote {
        (i: Int) => Some(i)
      }
      val o = OptionSome(Ident("i"))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "apply" in {
      inline def q = quote {
        (i: Int) => Option(i)
      }
      val o = OptionApply(Ident("i"))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "orNull" in {
      inline def q = quote {
        (o: Option[String]) => o.orNull
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionOrNull(Ident("o"))
    }
    "getOrNull" in {
      import extras._
      inline def q = quote {
        (o: Option[Int]) => o.getOrNull
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionGetOrNull(Ident("o"))
    }
    "None" in {
      inline def q = quote(None)
      val o = OptionNone(Quat.Null)
      quote(unquote(q)).ast mustEqual o
      repickle(o) mustEqual o
    }
    "forall" - {
      "simple" in {
        inline def q = quote {
          (o: Option[Boolean]) => o.forall(v => v)
        }
        val o = OptionForall(Ident("o"), Ident("v"), Ident("v"))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
      // TODO Option check on an entity should not compile. This should fail in the parser, see there for info.
      // "embedded" in {
      //   case class EmbeddedEntity(id: Int) extends Embedded
      //   "quote((o: Option[EmbeddedEntity]) => o.forall(v => v.id == 1))" mustNot compile
      // }
    }
    "exists" - {
      "simple" in {
        inline def q = quote {
          (o: Option[Boolean]) => o.exists(v => v)
        }
        val o = OptionExists(Ident("o"), Ident("v"), Ident("v"))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
      "unchecked" in {
        inline def q = quote {
          (o: Option[Row]) => o.exists(v => v.id == 4)
        }
        val o = OptionTableExists(Ident("o"), Ident("v"), Property(Ident("v"), "id") +==+ Constant.auto(4))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
      "embedded" in {
        case class EmbeddedEntity(id: Int) extends Embedded
        inline def q = quote {
          (o: Option[EmbeddedEntity]) => o.exists(v => v.id == 1)
        }
        val o = OptionTableExists(Ident("o"), Ident("v"), Property(Ident("v"), "id") +==+ Constant.auto(1))
        quote(unquote(q)).ast.asFunction.body mustEqual o
        repickle(o) mustEqual o
      }
    }
    "contains" in {
      inline def q = quote {
        (o: Option[Boolean], v: Int) => o.contains(v)
      }
      val o = OptionContains(Ident("o"), Ident("v"))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "isEmpty" in {
      inline def q = quote {
        (o: Option[Boolean]) => o.isEmpty
      }
      val o = OptionIsEmpty(Ident("o"))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "nonEmpty" in {
      inline def q = quote {
        (o: Option[Boolean]) => o.nonEmpty
      }
      val o = OptionNonEmpty(Ident("o"))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
    "isDefined" in {
      inline def q = quote {
        (o: Option[Boolean]) => o.isDefined
      }
      val o = OptionIsDefined(Ident("o"))
      quote(unquote(q)).ast.asFunction.body mustEqual o
      repickle(o) mustEqual o
    }
  }
}