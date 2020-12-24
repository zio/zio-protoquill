package scratch

import scala.quoted._


trait Foo(i: Int, j: String)
case class Impl(i: Int, j: String) extends Foo(i, j)

object TraitMatch {
  import io.getquill.parser.TastyMatchersContext

  inline def traitMatch[T](inline t: T): T = ${ traitMatchImpl[T]('t) }
  def traitMatchImpl[T: Type](t: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect._
    val tm = new TastyMatchersContext
    import tm._

    t match {
      //case '{ Impl($one, $two) } => println(s"Matched! Pulling ${one.show} and ${two.show}")
      
      // case  '{ ($f: Foo) } =>
      //   println("getting")
      //   println(f.asTerm.showExtractors)

      // Select(Ident(_), "apply"), List(Seal(one), Seal(two))
      case '{ (${Unseal(Apply(Select(Ident(id), "apply"), List(one, two)))}: Foo) } => 
        println("Getting")
        //println(a.showExtractors)
        //println(b.map(_.showExtractors))
        println(s"Args are: ${one.show} and ${two.show}")

      case _ => println("Not matched")
    }

    t
  }
}
