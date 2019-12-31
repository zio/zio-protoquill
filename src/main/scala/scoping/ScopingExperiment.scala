package scoping

import scala.quoted._

class InnerScoped {
  private val something = 3
  inline def getValue(i: Int): Int = ${InnerScoped.getValueImpl('i)}
  inline def innerCaller: Int = getValue(something)
}

class OuterScoped extends InnerScoped {
  inline def outerCaller: Int = innerCaller
  inline def outerCallerInfo(i: Int): Int = ${OuterScoped.outerCallerInfoImpl('i)}
}

object OuterScoped {
  def outerCallerInfoImpl(i: Expr[Int])(given qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty.{_, given}
    printer.ln(i.underlyingArgument.unseal)
    i
  }
}

object InnerScoped {
  def getValueImpl(i: Expr[Int])(given qctx: QuoteContext): Expr[Int] = '{$i + 1}
}