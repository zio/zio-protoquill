package miniquill.quoter

import scala.quoted._
import scala.quoted.matching._
import scala.collection.mutable.ArrayBuffer
import scala.quoted.util.ExprMap
import miniquill.dsl.GenericEncoder

case class Encodeable[PrepareRow](uid: String, value: Expr[Any], encoder: Expr[GenericEncoder[Any, PrepareRow]])

object FindEncodeables {

  def apply[PrepareRow: Type](input: Expr[Any])(given qctx: QuoteContext): List[Encodeable[PrepareRow]] = {
    import qctx.tasty.{given, _}

    ExprAccumulate(input) {

      // If it's an 'encodeable' vase, use the encoder directly

      // TODO Check that $prep is an instance of GenericEncoder[Any, PrepareRow], otherwise the cast shouldn't work and user should be warned
      case vase @ '{ ScalarEncodeableVase.apply[$tpe, $prep]($liftValue, $encoder, ${scala.quoted.matching.Const(uid: String)}) }  =>
        Encodeable(uid, liftValue, encoder.asInstanceOf[Expr[GenericEncoder[Any, PrepareRow]]])

      // Causes: this case is unreachable since class Tuple2 is not a subclass of class Expr
      // Not sure why. Probably language bug.
      //case `ScalarValueVase.apply`(liftValue, uid, _, tpe) =>
      case vase @ '{ ScalarValueVase.apply[$tpe]($liftValue, ${scala.quoted.matching.Const(uid: String)}) }  =>
        summonExpr(given '[GenericEncoder[$tpe, PrepareRow]]) match {
          case Some(encoder) => 
            // Need to case to Encoder[Any] since we can't preserve types
            // in the list that is comming out (and we don't need to keep track
            // of types once we have the decoders)
            Encodeable(uid, liftValue, encoder.asInstanceOf[Expr[GenericEncoder[Any, PrepareRow]]])
          // TODO Error case and good message when can't find encoder
        }
    }
  }
}



object FindLifts {

  def apply[T](input: Expr[Any])(given qctx: QuoteContext, tpe: quoted.Type[T]): List[(String, Expr[Any])] = {
    import qctx.tasty.{given, _}
    val quotationParser = new miniquill.parser.QuotationParser
    import quotationParser._

    ExprAccumulate(input) {
      // TODO block foldOver in this case?
      // NOTE that using this kind of pattern match, lifts are matched for both compile and run times
      // In compile times the entire tree of passed-in-quotations is matched including the 'lifts' 
      // (i.e. Quotation.lifts) tuples that are returned so we just get ScalarValueVase.apply
      // matched from those (as well as from the body of the passed-in-quotation but that's fine
      // since we dedupe by the UUID *). During runtime however, the actual case class instance
      // of ScalarTag is matched by the below term.

      // * That is to say if we have a passed-in-quotation Quoted(body: ... ScalarValueVase.apply, lifts: ..., (ScalarValueVase.apply ....))
      // both the ScalarValueVase in the body as well as the ones in the tuple would be matched. This is fine
      // since we dedupe the scalar value lifts by their UUID.


      case MatchLift(tree, uid) => (uid, tree)

      case MatchEncodeableLift(tree, uid) => (uid, tree)

      // If the quotation is runtime, it needs to be matched so that we can add it to the tuple
      // of lifts (i.e. runtime values) and the later evaluate it during the 'run' function.
      // Match the vase and add it to the list.
      
      case MatchRuntimeQuotation(tree, uid) => (uid, tree) // can't traverse inside here or errors happen
    }
  }
}