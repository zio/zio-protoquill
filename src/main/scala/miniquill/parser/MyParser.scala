package miniquill.parser 

import scala.quoted._

object MyParser {

  
  // // type Macro[T] = Quotes ?=> Expr[T]
  // // type Macro[T] = (using qctx: Quotes) => Expr[Unit]
  // type Macro[T] = (q: Quotes) ?=> Expr[T]

  // inline def example(inline any: Any): Unit = ${ myOperationParserImpl('any) }
  // def myOperationParserImpl(anyRaw: Expr[Any]): Macro[Unit] = {
  //   // 
  //   import quotes.reflect._

  //   val qctx = quotes // summon the current instance of 'quotes'
  //   import qctx.reflect._
  // }

  inline def myOperationParser(inline any: Any): Unit = ${ myOperationParserImpl('any) }
  def myOperationParserImpl(anyRaw: Expr[Any])(using qctx: Quotes): Expr[Unit] = {
    import qctx.reflect._
    val tm = new TastyMatchersContext
    import tm._

    object Unseal {
      def unapply(any: Expr[Any]): Option[Term] = Some(Term.of(any))
    }
    object Seal {
      def unapply(any: Term): Option[Expr[Any]] = Some(any.asExpr)
    }

    // Extractor:
    // Goes In: Expr[Any] 
    // Comes Out: 

    /*Goes In Expr[Any]*/ 
    val any = Term.of(anyRaw).underlyingArgument.asExpr

    def isLongOrInt(term: Term) = {
      term.tpe.widen <:< TypeRepr.of[Int] || term.tpe.widen <:< TypeRepr.of[Long]
    }


    // Does not work: case '{ type $tlong = Int | Long; ($left: `$tlong`).==($right: `$tlong`) } =>
    // Does work:     case '{ type $tlong <: Long; ($left: `$tlong`).==($right: `$tlong`) } =>

    val expr =
      any match {
        case '{ ($left: Long).==($right: Long) } =>
          println("YAY The Left and Right is an Int or Long ============== Using Quasai Quotes")

        case Unseal(Apply(Select(left /*Term*/, "=="), right :: Nil)) if isLongOrInt(left) && isLongOrInt(right) =>
          println("YAY The Left and Right is an Int or Long")

        case Unseal(Apply(Select(left /*Term*/, "=="), right :: Nil)) =>
          println(s"Left Is: ${left}, Right Is: ${right}")
          println(s"Type of Left Is: ${left.tpe.widen}")
          if (left.tpe.widen <:< TypeRepr.of[Int])
            println("YAY We matched")
          else
            println("Nope, we did not match")
        case _ =>
          println("Nope, did not match: " + Printer.TreeStructure.show(Term.of(any)))
      }

    
    
    '{ () }
  }
}