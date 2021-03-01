package io.getquill.metaprog

import scala.quoted._
import scala.quoted.{Const => ConstExpr, _}
import io.getquill.generic.GenericEncoder
import io.getquill.ast.Ast
import io.getquill.metaprog.ExtractorsBundle
import io.getquill.metaprog.ExtractorsBundle
import io.getquill.Quoted
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.ExprAccumulate
import io.getquill._

/* As the different kinds of parsing in Quill-Dotty became more complex, the need for an
overarching model of "how stuff works" became necessary. There are several places in the
Quill codebase where Dotty trees need to be parsed and their contents extracted, the most
noteable is the parser. However, a second important place is the methods that extract
lifts and quotations. This model is for the latter.

The conceptual model for this context is the following. A Dotty (i.e. Tasty) tree
is extracted from a quotation (a.k.a. "the ground") into either a Planter or a Vase.
When the tree matches a couple of criteria, it is considered to be "uprootable"
and can therefore be re-inserted into a Planter for the next quotation (as are lifts)
or consumed immediately as are inline Quotations. Otherwise, the tree is *not* re-insertable and it 
has to be "plucked" and inserted into a vase. The are still some conditions that this kind
of tree has to match so we call it "pluckable." Until we ascertain whether to re-insert
or pluck, the Tree is held (temporarily) inside of a Bin.

Different construcuts follow these rules in different ways. Scalar values for instances
cannot contain contents making them non-re-insertable and therefore are always
held inside of planters (i.e. the Planter) and replanted back into the part
of the tree constructing the PrepareRow in the 'run' method. 
Quotations are held in a QuotationLot until it is determined 
whether they are re-insertable. If they are, the Parser
will transparently read the AST through them. Otherwise they will be 'plucked' into a
QuotationVase and eventually processed during runtime.
*/
class ExprModel {
}


// TODO Change to 'replant' ?
// Plant the Planter back into the Scala AST
// Holds and parses variations of the Planter
sealed trait PlanterExpr[T: Type, PrepareRow: Type]:
  def uid: String
  def expr: Expr[T]
  def plant(using Quotes): Expr[Planter[T, PrepareRow]]

case class EagerPlanterExpr[T: Type, PrepareRow: Type](uid: String, expr: Expr[T], encoder: Expr[GenericEncoder[T, PrepareRow]]) extends PlanterExpr[T, PrepareRow]:
  def plant(using Quotes): Expr[EagerPlanter[T, PrepareRow]] =
    '{ EagerPlanter[T, PrepareRow]($expr, $encoder, ${Expr(uid)}) }

case class LazyPlanterExpr[T: Type, PrepareRow: Type](uid: String, expr: Expr[T]) extends PlanterExpr[T, PrepareRow]:
  def plant(using Quotes): Expr[LazyPlanter[T, PrepareRow]] =
    '{ LazyPlanter[T, PrepareRow]($expr, ${Expr(uid)}) }

object PlanterExpr {
  object Uprootable {
    def unapply(expr: Expr[Any])(using Quotes): Option[PlanterExpr[_, _]] = 
      import quotes.reflect._
      val tmc = new ExtractorsBundle
      import tmc._
      val e = UntypeExpr(expr)
      //println("@@@@@@@@@@@ Trying to match: " + Printer.TreeStructure.show(e.asTerm))

      UntypeExpr(expr) match {
        case '{ EagerPlanter.apply[qt, prep]($liftValue, $encoder, ${scala.quoted.Const(uid: String)}) } =>
          Some(EagerPlanterExpr[qt, prep](uid, liftValue, encoder/* .asInstanceOf[Expr[GenericEncoder[A, A]]] */).asInstanceOf[PlanterExpr[_, _]])
        case '{ LazyPlanter.apply[qt, prep]($liftValue, ${scala.quoted.Const(uid: String)}) } =>
          Some(LazyPlanterExpr[qt, prep](uid, liftValue).asInstanceOf[PlanterExpr[_, _]])
        case _ => 
          None
      }
  }

  protected object `(Planter).unquote` {
    def unapply(expr: Expr[Any])(using Quotes): Option[Expr[Planter[_, _]]] = expr match {
      case '{ ($planter: Planter[tt, pr]).unquote } => 
        Some(planter/* .asInstanceOf[Expr[Planter[A, A]]] */)
      case _ => 
        None
    }
  }


  object UprootableUnquote {
    def unapply(expr: Expr[Any])(using Quotes): Option[PlanterExpr[_, _]] = {
      import quotes.reflect.report
      expr match {
        case `(Planter).unquote`(planterUnquote) =>
          planterUnquote match {
            case Uprootable(planterExpr) => 
              Some(planterExpr)
            case _ => 
              // All lifts re-inserted as Planters must be inlined values containing
              // their UID as well as a corresponding tree. An error should be thrown if this is not the case.
              report.throwError("Format of ScalarLift holder must be fully inline.", expr)
          }
        case _ => None
      }
    }
  }


  def findUnquotes(expr: Expr[Any])(using Quotes): List[PlanterExpr[_, _]] =
    val res =
    ExprAccumulate(expr) {
      //case UprootableUnquote(planter) => planterx
      // Since we are also searching expressions inside spliced quotatations (in the Lifts slot) those things are not unquoted 
      // so we to search all planter expressions, not just the unquotes
      case PlanterExpr.Uprootable(planter) => planter
    }
    //println("((((((((((((((((((((((((((((((( Found Uprootable Unquotes ))))))))))))))))))))))\n"+res.map(_.expr.show))
    res

  // TODO Find a way to propogate PrepareRow into here
  // pull vases out of Quotation.lifts
  object UprootableList {
    def unapply(expr: Expr[List[Any]])(using Quotes): Option[List[PlanterExpr[_, _]]] = {
      import quotes.reflect._
      val tmc = new ExtractorsBundle
      import tmc._

      UntypeExpr(expr.asTerm.underlyingArgument.asExpr) match {
        case '{ Nil } =>
          Some(List())

        case '{ List.apply[t](${Varargs(elems)}: _*) } => 
          val scalarValues = 
            elems.collect {
              case PlanterExpr.Uprootable(vaseExpr) => vaseExpr
            }
          //println("****************** FIRST GOT HERE **************")
          //println(s"Scalar values (${scalarValues.length}): ${scalarValues.map(_.expr.show).mkString("(", ",", ")")}")
          //println(s"Elems (${elems.length}): ${elems.map(_.show).mkString("(", ",", ")")}")

          // if all the elements match SingleValueVase then return them, otherwise don't
          if (scalarValues.length == elems.length) Some(scalarValues.toList)
          else None

        case _ => 
          //println("~~~~~~~~ Tree is not Uprootable: " + Printer.TreeStructure.show(expr.asTerm))
          None
      }
    }
  }
}

case class QuotedExpr(ast: Expr[Ast], lifts: Expr[List[Planter[_, _]]], runtimeQuotes: Expr[List[QuotationVase]])
object QuotedExpr {
    //object `EmptyQuotationPouchList`

  // Note, the quotation is not considered to be inline if there are any runtime lifts
  object Uprootable {
    def unapply(expr: Expr[Any])(using Quotes): Option[QuotedExpr] = {
      import quotes.reflect.{Term => QTerm, _}
      val tmc = new ExtractorsBundle
      import tmc._

      expr match {
        /* No runtime lifts allowed for inline quotes so quotationPouches.length must be 0 */
        case exprr @  '{ Quoted.apply[qt]($ast, $v, Nil) } =>  //List.apply[ttt](${GenericSeq(args)}: _*) if (args.length == 0)
          Some(QuotedExpr(ast, v, '{ Nil }))
        case TypedMatroshka(tree) => 
          Uprootable.unapply(tree)
        case _ => 
          None
      }
    }
  }

  object UprootableWithLifts {
    def unapply(expr: Expr[Any])(using Quotes): Option[(QuotedExpr, List[PlanterExpr[_, _]])] =
      expr match {
        case QuotedExpr.Uprootable(quotedExpr @ QuotedExpr(ast, PlanterExpr.UprootableList(lifts), _)) => 
          Some((quotedExpr, lifts))
        case _ => 
          None
      }
  }

  def uprootableWithLiftsOpt(quoted: Expr[Any])(using Quotes): Option[(QuotedExpr, List[PlanterExpr[_, _]])] = 
    import quotes.reflect._
    quoted match {
      case QuotedExpr.UprootableWithLifts(quotedExpr) => Some(quotedExpr)
      case _ => 
        println("Quotations with Lifts do not meet compiletime criteria: " + Printer.TreeShortCode.show(quoted.asTerm)); 
        None
    }

  // Does the Quoted expression match the correct format needed for it to
  // be inlineable i.e. transpileable into a Query during Compile-Time.
  def uprootableOpt(quoted: Expr[Any])(using Quotes): Option[QuotedExpr] = 
    import quotes.reflect._
    quoted match {
      case QuotedExpr.Uprootable(quotedExpr) => Some(quotedExpr)
      case _ => 
        println("Quotations do not meet compiletime criteria: " + Printer.TreeShortCode.show(quoted.asTerm)); 
        None
    }
}



sealed trait QuotationLotExpr
object QuotationLotExpr {

  protected object `(QuotationLot).unquote` {
    def unapply(expr: Expr[Any])(using Quotes) = {
      import quotes.reflect._
      val tm = new ExtractorsBundle
      import tm._
      UntypeExpr(expr) match {
        // When a QuotationLot is embedded into an ast
        case '{ (${quotationLot}: QuotationLot[tt]).unquote } => 
          Some(quotationLot)
        
        // TODO Now since we have UntypeExpr this below might not be needed
        // There are situations e.g. SchemaMeta where there's an additional type ascription needed
        // there it needs to be specified in the AST manually. Maybe this is a bug?
        case '{ type tt; ((${quotationLot}: QuotationLot[`tt`]).unquote: `tt`) } => 
          Some(quotationLot)

        case other => 
          None
      }
    }
  }

  /**
  * Match all of the different kinds of QuotationLots and unpack their contents.
  */
  protected object `QuotationLot.apply` {
    
    def unapply(expr: Expr[Any])(using Quotes): Option[(Expr[Quoted[Any]], String, List[Expr[_]])] = {
      import quotes.reflect._
      val tm = new ExtractorsBundle
      import tm._
      UntypeExpr(expr) match {
        // Extract the entity, the uid and any other expressions the qutation bin may have 
        // (e.g. the extractor if the QuotationLot is a QueryMeta)

        case '{ Unquote.apply[t]($quotation, ${ConstExpr(uid: String)}) } =>
          Some((quotation, uid, List()))

        case '{ SchemaMeta.apply[t]($quotation, ${ConstExpr(uid: String)}) } =>
          Some((quotation, uid, List()))

        case '{ InsertMeta.apply[t]($quotation, ${ConstExpr(uid: String)}) } =>
          Some((quotation, uid, List()))

        case '{ CaseClassLift.apply[t]($quotation, ${ConstExpr(uid: String)}) } =>
          Some((quotation, uid, List()))

        case '{ QueryMeta.apply[t, r]($quotation, ${ConstExpr(uid: String)}, $extractor) } =>
          Some((quotation, uid, List(extractor)))

        case other =>
          None
      }
    }
  }

  object findUnquotes {
    def apply(expr: Expr[Any])(using Quotes) =
      ExprAccumulate(expr) {
        case QuotationLotExpr.Unquoted(vaseExpr) => 
          vaseExpr
      }
  }

  object Unquoted {
    def apply(expr: Expr[Any])(using Quotes): QuotationLotExpr =
      unapply(expr).getOrElse { quotes.reflect.report.throwError(s"The expression: ${expr.show} is not a valid unquotation of a Quoted Expression (i.e. a [quoted-expression].unqoute) and cannot be unquoted.") }

    def unapply(expr: Expr[Any])(using Quotes): Option[QuotationLotExpr] = 
      //println("=================== Unapplying Unquote ===================")
      //println(io.getquill.Format(expr.show))
      expr match {
        case `(QuotationLot).unquote`(QuotationLotExpr(vaseExpr)) => 
          //println("=============== MATCHED ===============")
          Some(vaseExpr)
        case _ => 
          //println("=============== NOT MATCHED ===============")
          None
      }
  }

  def apply(expr: Expr[Any])(using Quotes): QuotationLotExpr =
    unapply(expr).getOrElse { quotes.reflect.report.throwError(s"The expression: ${expr.show} is not a valid Quoted Expression and cannot be unquoted.") }

  // Verify that a quotation is inline. It is inline if all the lifts are inline. There is no need
  // to search the AST since it has been parsed already
  def unapply(expr: Expr[Any])(using Quotes): Option[QuotationLotExpr] = {
    import quotes.reflect._

    
    expr match {
      case vase @ `QuotationLot.apply`(quoted @ QuotedExpr.Uprootable(ast, PlanterExpr.UprootableList(lifts), _), uid, rest) => // TODO Also match .unapply?
        Some(Uprootable(uid, ast, vase.asInstanceOf[Expr[QuotationLot[Any]]], quoted, lifts, rest))

      case ql @ `QuotationLot.apply`(quotation, uid, rest) =>
        //println("======= Only Pluckable: " + Printer.TreeShortCode.show(ql.asTerm))
        Some(Pluckable(uid, quotation, rest))

      // If it's a QuotationLot but we can't extract it at all, need to throw an error
      case '{ ($qb: QuotationLot[t]) } =>
        Some(Pointable(qb))

      case _ => 
        None
    }
  }

  case class Pointable(expr: Expr[QuotationLot[Any]]) extends QuotationLotExpr

  /**
   * QuotationLots that have runtime values hance cannot be re-planted into the scala AST and
   * they need to be put into QuotationVasees.
   * The 'other' argument is meant to be used in various unique circumstances. Right now it
   * is just used by a QueryMeta to carry an extractor function that contra-maps back to the T type
   */
  case class Pluckable(uid: String, expr: Expr[Quoted[Any]], other: List[Expr[_]]) extends QuotationLotExpr {
    def pluck(using Quotes) = '{ QuotationVase($expr, ${Expr(uid)}) }
  }

  // QuotationLots expressions that can be further inlined into quotated clauses


  // Alternative more-compact representation of Uprootable, look into doing this
  // {
  //   class Uprootable(
  //     val uid: String, 
  //     val ast: Expr[Ast], 
  //     val inlineLifts: List[PlanterExpr[_, _]]
  //   )(
  //     val bin: Expr[QuotationLot[Any]], 
  //     val quotation: Expr[Quoted[Any]],
  //     val rest: List[Expr[_]]
  //   )

  //   object Uprootable {
  //     def unapply(value: Uprootable): Option[(String, Expr[Ast], List[PlanterExpr[_, _]])] =
  //       Some((value.uid, value.ast, value.inlineLifts))
      
  //     object Full {
  //       def unapply(value: Uprootable): Option[(
  //         uid: String, 
  //         ast: Expr[Ast],
  //         bin: Expr[QuotationLot[Any]], 
  //         quotation: Expr[Quoted[Any]],
  //         inlineLifts: List[PlanterExpr[_, _]],
  //         rest: List[Expr[_]]
  //       )] = Some((value.uid, value.ast, value.bin, value.quotation, value.inlineLifts, value.rest))
  //     }
  //   }
  // }

  case class Uprootable(
    uid: String, 
    ast: Expr[Ast],
    bin: Expr[QuotationLot[Any]], 
    quotation: Expr[Quoted[Any]],
    inlineLifts: List[PlanterExpr[_, _]],
    rest: List[Expr[_]]
  ) extends QuotationLotExpr
}

// This allows anyone who imports io.getquill automatically bring in QuotationLot subclasses
export QuotationLotExpr.Pointable
export QuotationLotExpr.Pluckable
export QuotationLotExpr.Uprootable
