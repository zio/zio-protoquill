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

class OptionOperationTest extends Spec with Inside {
  "option operation" - {
    import io.getquill.ast.Implicits._

    case class Row(id: Int, value: String)

    "map" - {
      "simple" in {
        inline def q = quote {
          (o: Option[Int]) => o.map(v => v)
        }
        quote(unquote(q)).ast.asFunction.body mustEqual OptionMap(Ident("o"), Ident("v"), Ident("v"))
      }
      "unchecked" in {
        inline def q = quote {
          (o: Option[Row]) => o.map(v => v)
        }
        quote(unquote(q)).ast.asFunction.body mustEqual OptionTableMap(Ident("o"), Ident("v"), Ident("v"))
      }
    }
    "flatMap" - {
      "simple" in {
        inline def q = quote {
          (o: Option[Int]) => o.flatMap(v => Option(v))
        }
        quote(unquote(q)).ast.asFunction.body mustEqual OptionFlatMap(Ident("o"), Ident("v"), OptionApply(Ident("v")))
      }
      "unchecked" in {
        inline def q = quote {
          (o: Option[Row]) => o.flatMap(v => Option(v))
        }
        quote(unquote(q)).ast.asFunction.body mustEqual OptionTableFlatMap(Ident("o"), Ident("v"), OptionApply(Ident("v")))
      }
    }
    "getOrElse" in {
      inline def q = quote {
        (o: Option[Int]) => o.getOrElse(11)
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionGetOrElse(Ident("o"), Constant.auto(11))
    }
    "map + getOrElse" in {
      inline def q = quote {
        (o: Option[Int]) => o.map(i => i < 10).getOrElse(true)
      }
      quote(unquote(q)).ast.asFunction.body mustEqual
        OptionGetOrElse(
          OptionMap(Ident("o"), Ident("i"), BinaryOperation(Ident("i"), NumericOperator.`<`, Constant.auto(10))),
          Constant.auto(true)
        )
    }
    "flatten" in {
      inline def q = quote {
        (o: Option[Option[Int]]) => o.flatten
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionFlatten(Ident("o"))
    }
    "Some" in {
      inline def q = quote {
        (i: Int) => Some(i)
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionSome(Ident("i"))
    }
    "apply" in {
      inline def q = quote {
        (i: Int) => Option(i)
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionApply(Ident("i"))
    }
    // TODO Need to import extras and write parser for them
    // "orNull" in {
    //   inline def q = quote {
    //     (o: Option[String]) => o.orNull
    //   }
    //   quote(unquote(q)).ast.asFunction.body mustEqual OptionOrNull(Ident("o"))
    // }
    // TODO Need to import extras and write parser for them
    // "getOrNull" in {
    //   inline def q = quote {
    //     (o: Option[Int]) => o.getOrNull
    //   }
    //   quote(unquote(q)).ast.asFunction.body mustEqual OptionGetOrNull(Ident("o"))
    // }
    "None" in {
      inline def q = quote(None)
      quote(unquote(q)).ast mustEqual OptionNone(Quat.Null)
    }
    "forall" - {
      "simple" in {
        inline def q = quote {
          (o: Option[Boolean]) => o.forall(v => v)
        }
        quote(unquote(q)).ast.asFunction.body mustEqual OptionForall(Ident("o"), Ident("v"), Ident("v"))
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
        quote(unquote(q)).ast.asFunction.body mustEqual OptionExists(Ident("o"), Ident("v"), Ident("v"))
      }
      "unchecked" in {
        inline def q = quote {
          (o: Option[Row]) => o.exists(v => v.id == 4)
        }
        quote(unquote(q)).ast.asFunction.body mustEqual OptionTableExists(Ident("o"), Ident("v"), Property(Ident("v"), "id") +==+ Constant.auto(4))
      }
      "embedded" in {
        case class EmbeddedEntity(id: Int) extends Embedded
        inline def q = quote {
          (o: Option[EmbeddedEntity]) => o.exists(v => v.id == 1)
        }
        quote(unquote(q)).ast.asFunction.body mustEqual OptionTableExists(Ident("o"), Ident("v"), Property(Ident("v"), "id") +==+ Constant.auto(1))
      }
    }
    "contains" in {
      inline def q = quote {
        (o: Option[Boolean], v: Int) => o.contains(v)
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionContains(Ident("o"), Ident("v"))
    }
    "isEmpty" in {
      inline def q = quote {
        (o: Option[Boolean]) => o.isEmpty
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionIsEmpty(Ident("o"))
    }
    "nonEmpty" in {
      inline def q = quote {
        (o: Option[Boolean]) => o.nonEmpty
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionNonEmpty(Ident("o"))
    }
    "isDefined" in {
      inline def q = quote {
        (o: Option[Boolean]) => o.isDefined
      }
      quote(unquote(q)).ast.asFunction.body mustEqual OptionIsDefined(Ident("o"))
    }
  }
}
