package errorcase

import scala.quoted._

object ErrorCaseExperiment {
  inline def stringOrError[T](str: T): T = ${ stringOrErrorImpl[T]('str) }
  def stringOrErrorImpl[T: Type](str: Expr[T])(using qctx: QuoteContext): Expr[T] = {
    import qctx.reflect._
    str match {
      case '{ ($s: String) } => s
      case _ => report.error("Not a string", str); '{???} //throw new RuntimeException()
    }
  }
}