package miniquill.parser 

import scala.quoted._

object MyParser {
  inline def myOperationParser(inline any: Any): Unit = ${ myOperationParserImpl('any) }
  def myOperationParserImpl(anyRaw: Expr[Any])(using qctx: QuoteContext): Expr[Unit] = {
    import qctx.reflect._
    val tm = new TastyMatchersContext
    import tm._

    object Unseal {
      def unapply(any: Expr[Any]): Option[Term] = Some(any.unseal)
    }
    object Seal {
      def unapply(any: Term): Option[Expr[Any]] = Some(any.seal)
    }

    // Extractor:
    // Goes In: Expr[Any] 
    // Comes Out: 

    /*Goes In Expr[Any]*/ 
    val any = anyRaw.unseal.underlyingArgument.seal

    def isLongOrInt(term: Term) = {
      term.tpe.widen <:< '[Int].unseal.tpe || term.tpe.widen <:< '[Long].unseal.tpe
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
          if (left.tpe.widen <:< '[Int].unseal.tpe)
            println("YAY We matched")
          else
            println("Nope, we did not match")
        case _ =>
          println("Nope, did not match: " + any.unseal.showExtractors)
      }

    
    
    '{ () }
  }
}