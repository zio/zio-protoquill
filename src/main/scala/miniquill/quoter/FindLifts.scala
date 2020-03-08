package miniquill.quoter

import scala.quoted._
import scala.quoted.matching._
import scala.collection.mutable.ArrayBuffer
import scala.quoted.util.ExprMap
import miniquill.dsl.GenericEncoder

case class Encodeable(uid: String, value: Any, encoder: GenericEncoder[Any, Any])

// InlineEncodeable should be a ScalarPlanterExpr
case class InlineEncodeable(uid: String, value: Expr[Any], encoder: Expr[GenericEncoder[Any, Any]], vase: Expr[ScalarPlanter[_, _]]) {
  def prepare(given qctx: QuoteContext): Expr[Encodeable] = {
    '{ Encodeable(${Expr(uid)}, $value, $encoder) }
  }
}


// If all encodeables are compile-time return the list of their value, otherwise return false
object InlineEncodeables {

  // TODO PrepareRow should be injected into the matches? (or they should be checked to be this type
  // otherwise it's invalid and an exception should be thrown?)

  // Remove the unapply pattern here. InlineEncodeables should be 
  def unapply(input: Expr[Any])(given qctx: QuoteContext): Option[List[InlineEncodeable]] = {
    import qctx.tasty.{given, _}

    val matchedEncodeables =
      ExprAccumulate(input) {

        // TODO Check that $prep is an instance of GenericEncoder[Any, PrepareRow], otherwise the cast shouldn't work and user should be warned
        case vase @ '{ ScalarPlanter.apply[$tpe, $prep]($liftValue, $encoder, ${scala.quoted.matching.Const(uid: String)}) }  =>
          Some(InlineEncodeable(uid, liftValue, encoder.asInstanceOf[Expr[GenericEncoder[Any, Any]]], vase))

        // If it's a reference but not inline
        case '{ ($sv: ScalarPlanter[$tpe, $prep]) } =>
          qctx.throwError("Invalid Lift found. A lift must be directly inlined and cannot be a previously computed runtime value.", input)
      }

    matchedEncodeables.forall(_.isDefined) match {
      case true => Some(matchedEncodeables.collect { case Some(value) => value })
      case false => None
    }
  }
}




object FindRuntimeQuotationBins {

  def apply[T](input: Expr[Any])(given qctx: QuoteContext, tpe: quoted.Type[T]): List[(String, Expr[QuotationBin[Any]])] = {
    import qctx.tasty.{given, _}
    val quotationParser = new miniquill.parser.QuotationParser
    import quotationParser._

    ExprAccumulate(input) {

      // If the quotation is runtime, it needs to be matched so that we can add it to the tuple
      // of lifts (i.e. runtime values) and the later evaluate it during the 'run' function.
      // Match the vase and add it to the list.
      
      case MatchRuntimeQuotationBins(tree, uid) => (uid, tree) // can't traverse inside here or errors happen
    }
  }
}