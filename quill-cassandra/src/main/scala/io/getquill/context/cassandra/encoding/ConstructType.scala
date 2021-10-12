package io.getquill.context.cassandra.encoding

import scala.quoted._
import scala.deriving._
import io.getquill.util.Format

object ConstructType:
  def apply[T: Type](m: Expr[Mirror.ProductOf[T]], children: List[(Type[_], Expr[_])])(using Quotes) =
    import quotes.reflect._
    //println(s"---- Constructing Type for: ${Format.TypeOf[T]}")

    val types = children.map(_._1)
    val terms = children.map(_._2)
    // Get the constructor
    val tpe = TypeRepr.of[T]
    val constructor = TypeRepr.of[T].typeSymbol.primaryConstructor

    //println(s"tpe: ${tpe}, constructor: ${constructor}")

    // If we are a tuple, we can easily construct it
    if (tpe <:< TypeRepr.of[Tuple]) {
      val construct =
        Apply(
          TypeApply(
            Select(New(TypeTree.of[T]), constructor),
            types.map { tpe =>
              tpe match
                case '[tt] => TypeTree.of[tt]
            }
          ),
          terms.map(_.asTerm)
        )
      //println(s"=========== Create from Tuple Constructor ${Format.Expr(construct.asExprOf[T])} ===========")
      construct.asExprOf[T]
    // If we are a case class with no generic parameters, we can easily construct it
    } else if (tpe.classSymbol.exists(_.flags.is(Flags.Case)) && !constructor.paramSymss.exists(_.exists(_.isTypeParam))) {
      val construct =
        Apply(
          Select(New(TypeTree.of[T]), constructor),
          terms.map(_.asTerm)
        )
      //println(s"=========== Create from CaseClass Constructor ${Format.Expr(construct.asExprOf[T])} ===========")
      construct.asExprOf[T]
    // Otherwise, we have to use the mirror itself for construction which is more spliced code
    } else {
      //println(s"=========== Create from Mirror ${Format.Expr(m)} ===========")
      '{ $m.fromProduct(${Expr.ofTupleFromSeq(terms)}) }.asExprOf[T]
    }
  end apply
end ConstructType