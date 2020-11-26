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
  def outerCallerInfoImpl(i: Expr[Any])(using Quotes): Expr[Any] = {
    import quotes.reflect._
    printer.lnf(i.show)
    i
  }
}

object InnerScoped {
  def getValueImpl(i: Expr[Any])(using Quotes): Expr[Any] = '{$i}
}