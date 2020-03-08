package miniquill.parser

import scala.quoted._

class SealUnseal(given val qctx: QuoteContext) {
  import qctx.tasty.{given, _}

  object Unseal {
    def unapply(t: Expr[Any]) = {
      Some(t.unseal)
    }
  }
  object Seal {
    def unapply[T](e: Term) = {
      implicit val ttpe: quoted.Type[T] = e.tpe.seal.asInstanceOf[quoted.Type[T]]
      Some(e.seal.cast[T])
    }
  }
}