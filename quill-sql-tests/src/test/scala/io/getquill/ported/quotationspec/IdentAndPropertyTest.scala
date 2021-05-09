package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quat.Quat
import io.getquill._

class IdentAndPropertyTest extends Spec with TestEntities {
  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")

  "ident" in {
    val q = quote {
      (s: String) => s
    }
    quote(unquote(q)).ast.body mustEqual Ident("s")
  }
  "property" - {
    "class field" in {
      val q = quote {
        qr1.map(t => t.s)
      }
      quote(unquote(q)).ast.body mustEqual Property(Ident("t"), "s")
    }
    "option.get fails" in {
      """
        quote {
          (o: Option[Int]) => o.get
        }
      """ mustNot compile
    }
    "fails if not case class property" - {
      "val" in {
        case class T(s: String) {
          val boom = 1
        }
        """
        quote {
          (o: T) => o.boom
        }
        """ mustNot compile
      }
      "def" in {
        case class T(s: String) {
          def boom = 1
        }
        """
        quote {
          (o: T) => o.boom
        }
        """ mustNot compile
      }
    }
  }
  "property anonymous" in {
    val q = quote {
      qr1.map(t => t.s)
    }
    quote(unquote(q)).ast.body mustEqual Property(Ident("t"), "s")
  }
}
