package miniquill.parser.dynamictest

import scala.quoted._
import miniquill.parser.PrintMac

case class Person(name: String)
case class Car(p: Person, otherValue: String) {
  // If you return Car.p, it will optimize away the Car part and just pull out p
  def pThrow: Person = ???
}
case class CarProxy(p: List[Person], otherValue: String)

object Test {

  class Ops(using val quotes: Quotes) {
    import quotes.reflect._
    object UntypeExpr {
      def untypeRecurse(term: Term): Term = 
        term match
          case Typed(x, _) => 
            println("Untping into: " + x.showExtractors)
            untypeRecurse(x)
          case other => 
            println("Skipping Untype of: " + other.showExtractors)
            other

      def unapply(expr: Expr[Any]) =
        val und = Term.of(expr).underlyingArgument 
        Some(untypeRecurse(und).asExpr)

      def apply(expr: Expr[Any]) = unapply(expr).get
    }
  }

  inline def macroSplice(inline any: Person): Person = ${ macroSpliceImpl('any) }
  def macroSpliceImpl(expr: Expr[Person])(using quotes: Quotes): Expr[Person] = {
    import quotes.reflect._
    val foo = "foo"
    val output = '{ Car($expr, ${Expr(foo)}).pThrow }
    //println("====== Returning Output ======")
    //PrintMac.printMacImpl(output)
    output
  }

  inline def macroPull(inline any: Person): CarProxy = ${ macroPullImpl('any) }
  def macroPullImpl(exprRaw: Expr[Person])(using quotes: Quotes): Expr[CarProxy] = {
    import quotes.reflect._
    val ops = new Ops
    import ops._
    val expr = UntypeExpr(exprRaw)
    println("Expr is now: " + Term.of(expr).showExtractors)

    val carOfPerson =
      expr match {
        case '{ ($car: Car).pThrow } => car
        case _ => report.throwError("Could not match Car.pThrow from: " + Term.of(expr).show)
      }

    val p = 
      carOfPerson match {
        case UntypeExpr('{ Car.apply($p, $v) }) => p
        case _ => report.throwError("Could not match Car.apply from: " + Term.of(carOfPerson).show)
      }

    val people = '{ List(${Varargs(Seq(p, p))}: _*) }
    '{ CarProxy($people, "foofoo") }
  }


  // Maybe it needs to be an array, maybe lifting via Exprs is broken?
  inline def macroUse(inline any: CarProxy): Person = ${ macroUseImpl('any) }
  def macroUseImpl(anyRaw: Expr[CarProxy])(using quotes: Quotes): Expr[Person] = {
    import quotes.reflect._
    val ops = new Ops
    import ops._
    // val any = UntypeExpr(anyRaw)
    // val carProxy =
    //   any match {
    //     case '{ ($carProxy: CarProxy).p } => carProxy
    //     case _ => report.throwError("Could not match CarProxy.p")
    //   }
    val any = UntypeExpr(anyRaw).asInstanceOf[Expr[CarProxy]]

    '{ inlineUse($any) }
  }

  def inlineUse(any: CarProxy) = {
    any.p(0)
  }
}