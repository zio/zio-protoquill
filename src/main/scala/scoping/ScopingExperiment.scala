package scoping

import scala.quoted._

class InnerScoped {
  private val something = 3
  inline def getValue(i: Any): Any = ${InnerScoped.getValueImpl('i)}
  inline def innerCaller: Any = getValue(something)
}

class OuterScoped extends InnerScoped {
  inline def outerCaller: Any = innerCaller
  inline def outerCallerInfo(inline i: Any): Any = ${OuterScoped.outerCallerInfoImpl('i)}
}

object OuterScoped {
  def outerCallerInfoImpl(i: Expr[Any])(given qctx: QuoteContext): Expr[Any] = {
    import qctx.tasty.{_, given _}
    printer.lnf(i.show)
    i
  }
}

object InnerScoped {
  def getValueImpl(i: Expr[Any])(given qctx: QuoteContext): Expr[Any] = '{$i}
}