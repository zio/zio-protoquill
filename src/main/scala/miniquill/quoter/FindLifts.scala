package miniquill.quoter

import scala.quoted._
import scala.quoted.matching._
import scala.collection.mutable.ArrayBuffer
import scala.quoted.util.ExprMap

object FindLifts {

  def apply[T](input: Expr[Any])(given qctx: QuoteContext, tpe: quoted.Type[T]): List[(String, Expr[Any])] = {
    import qctx.tasty.{given, _}

    val quotationParser = new miniquill.parser.QuotationParser
    import quotationParser._

    val buff: ArrayBuffer[(String, Expr[Any])] = new ArrayBuffer[(String, Expr[Any])]()
    val accum = new ExprMap {
      def transform[T](expr: Expr[T])(given qctx: QuoteContext, tpe: quoted.Type[T]): Expr[T] = {

        expr match {
          // TODO block foldOver in this case?
          // NOTE that using this kind of pattern match, lifts are matched for both compile and run times
          // In compile times the entire tree of passed-in-quotations is matched including the 'lifts' 
          // (i.e. Quotation.lifts) tuples that are returned so we just get ScalarValueVase.apply
          // matched from those (as well as from the body of the passed-in-quotation but that's fine
          // since we dedupe by the UUID *). During runtime however, the actual case class instance
          // of ScalarValueTag is matched by the below term.

          // * That is to say if we have a passed-in-quotation Quoted(body: ... ScalarValueVase.apply, lifts: ..., (ScalarValueVase.apply ....))
          // both the ScalarValueVase in the body as well as the ones in the tuple would be matched. This is fine
          // since we dedupe the scalar value lifts by their UUID.

          // TODO Why can't this be parsed with *: operator?
          case '{ ScalarValueVase($tree, ${Const(uid)}) } => 
            buff += ((uid, expr))
            expr // can't go inside here or errors happen

          // If the quotation is runtime, it needs to be matched so that we can add it to the tuple
          // of lifts (i.e. runtime values) and the later evaluate it during the 'run' function.
          // Match the vase and add it to the list.
          
          // This doesn't seem to work
          case MatchRuntimeQuotation(tree, uid) => // can't go inside here or errors happen
            buff += ((uid, tree))
            expr // can't go inside here or errors happen

          // case '{ QuotationVase.apply[$t]($tree, ${Const(uid)}) } =>
          //   buff += ((uid, tree))
          //   expr // can't go inside here or errors happen

          case other =>
            expr
            //transformChildren(expr)
        }

        expr.unseal match {
          // Not including this causes execption "scala.tasty.reflect.ExprCastError: Expr: [ : Nothing]" in certain situations
          case Repeated(Nil, Inferred()) => expr 
          case _ => transformChildren[T](expr)
        }
      }
    }

    accum.transform(input) // check if really need underlyingArgument

    buff.toList
  }
}