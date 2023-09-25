package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quat.Quat
import io.getquill._
import io.getquill.PicklingHelper._

class IdentAndPropertyTest extends Spec with TestEntities {
  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case f: Map      => f.body
      case _ =>
        throw new IllegalArgumentException(
          s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}"
        )

  "ident" in {
    inline def q = quote { (s: String) =>
      s
    }
    quote(unquote(q)).ast.body mustEqual Ident("s")
  }
  "property" - {
    "class field" in {
      inline def q = quote {
        qr1.map(t => t.s)
      }
      val p = Property(Ident("t"), "s")
      quote(unquote(q)).ast.body mustEqual p
      repickle(p) mustEqual p
    }
    // TODO Need to introduce clauses in the parser to fail on the below constructs
    // "option.get fails" in {
    //   """
    //     quote {
    //       (o: Option[Int]) => o.get
    //     }
    //   """ mustNot compile
    // }
    // "fails if not case class property" - {
    //   "val" in {
    //     case class T(s: String) {
    //       val boom = 1
    //     }
    //     """
    //     quote {
    //       (o: T) => o.boom
    //     }
    //     """ mustNot compile
    //   }
    //   "def" in {
    //     case class T(s: String) {
    //       def boom = 1
    //     }
    //     """
    //     quote {
    //       (o: T) => o.boom
    //     }
    //     """ mustNot compile
    //   }
    // }
  }
  "property anonymous" in {
    inline def q = quote {
      qr1.map(t => t.s)
    }
    val p = Property(Ident("t"), "s")
    quote(unquote(q)).ast.body mustEqual p
    repickle(p) mustEqual p
  }
}
