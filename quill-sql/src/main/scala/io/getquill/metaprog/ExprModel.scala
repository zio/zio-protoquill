package io.getquill.metaprog

import scala.quoted._
import io.getquill.generic.GenericEncoder
import io.getquill.ast.Ast
import io.getquill.metaprog.Extractors._
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.ExprAccumulate
import io.getquill.util.Format
import io.getquill._
import io.getquill.parser.Lifter
import io.getquill.parser.Unlifter

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

Different constructs follow these rules in different ways. Scalar values for instances
cannot contain contents making them non-re-insertable and therefore are always
held inside of planters (i.e. the Planter) and replanted back into the part
of the tree constructing the PrepareRow in the 'run' method.
Quotations are held in a QuotationLot until it is determined
whether they are re-insertable. If they are, the Parser
will transparently read the AST through them. Otherwise they will be 'plucked' into a
QuotationVase and eventually processed during runtime.
 */
class ExprModel {}

// This is the mirror of `Planter`. It holds types of
// Planters and allows planting them back into the Scala AST
// (need scala.quoted.Type here i.e. full name or incremental recompile breaks)
sealed trait PlanterExpr[T: scala.quoted.Type, PrepareRow: scala.quoted.Type, Session: scala.quoted.Type]:
  def uid: String
  def plant(using Quotes): Expr[Planter[T, PrepareRow, Session]] // TODO Change to 'replant' ?
  def nestInline(using Quotes)(call: Option[quotes.reflect.Tree], bindings: List[quotes.reflect.Definition]): PlanterExpr[T, PrepareRow, Session]

case class EagerListPlanterExpr[T, PrepareRow: Type, Session: Type](uid: String, expr: Expr[List[T]], encoder: Expr[GenericEncoder[T, PrepareRow, Session]])(using val tpe: Type[T], queryTpe: Type[Query[T]])
    extends PlanterExpr[Query[T], PrepareRow, Session]:
  def plant(using Quotes): Expr[EagerListPlanter[T, PrepareRow, Session]] =
    '{ EagerListPlanter[T, PrepareRow, Session]($expr, $encoder, ${ Expr(uid) }) }
  def nestInline(using Quotes)(call: Option[quotes.reflect.Tree], bindings: List[quotes.reflect.Definition]) =
    import quotes.reflect._
    this.copy[T, PrepareRow, Session](
      expr = Inlined(call, bindings, this.expr.asTerm).asExprOf[List[T]],
      encoder = Inlined(call, bindings, this.encoder.asTerm).asExprOf[GenericEncoder[T, PrepareRow, Session]]
    )

case class EagerPlanterExpr[T: Type, PrepareRow: Type, Session: Type](uid: String, expr: Expr[T], encoder: Expr[GenericEncoder[T, PrepareRow, Session]]) extends PlanterExpr[T, PrepareRow, Session]:
  def plant(using Quotes): Expr[EagerPlanter[T, PrepareRow, Session]] =
    '{ EagerPlanter[T, PrepareRow, Session]($expr, $encoder, ${ Expr(uid) }) }
  def nestInline(using Quotes)(call: Option[quotes.reflect.Tree], bindings: List[quotes.reflect.Definition]) =
    import quotes.reflect._
    this.copy[T, PrepareRow, Session](
      expr = Inlined(call, bindings, this.expr.asTerm).asExprOf[T],
      encoder = Inlined(call, bindings, this.encoder.asTerm).asExprOf[GenericEncoder[T, PrepareRow, Session]]
    )

case class InjectableEagerPlanterExpr[T: Type, PrepareRow: Type, Session: Type](uid: String, inject: Expr[_ => T], encoder: Expr[GenericEncoder[T, PrepareRow, Session]]) extends PlanterExpr[T, PrepareRow, Session]:
  def plant(using Quotes): Expr[InjectableEagerPlanter[T, PrepareRow, Session]] =
    '{ InjectableEagerPlanter[T, PrepareRow, Session]($inject, $encoder, ${ Expr(uid) }) }
  def inject(injectee: Expr[Any])(using Quotes): Expr[EagerPlanter[T, PrepareRow, Session]] =
    '{ EagerPlanter[T, PrepareRow, Session]($inject.asInstanceOf[Any => T].apply($injectee), $encoder, ${ Expr(uid) }) }
  def nestInline(using Quotes)(call: Option[quotes.reflect.Tree], bindings: List[quotes.reflect.Definition]) =
    import quotes.reflect._
    this.copy[T, PrepareRow, Session](
      inject = Inlined(call, bindings, this.inject.asTerm).asExprOf[_ => T],
      encoder = Inlined(call, bindings, this.encoder.asTerm).asExprOf[GenericEncoder[T, PrepareRow, Session]]
    )

case class LazyPlanterExpr[T: Type, PrepareRow: Type, Session: Type](uid: String, expr: Expr[T]) extends PlanterExpr[T, PrepareRow, Session]:
  def plant(using Quotes): Expr[LazyPlanter[T, PrepareRow, Session]] =
    '{ LazyPlanter[T, PrepareRow, Session]($expr, ${ Expr(uid) }) }
  def nestInline(using Quotes)(call: Option[quotes.reflect.Tree], bindings: List[quotes.reflect.Definition]) =
    import quotes.reflect._
    this.copy[T, PrepareRow, Session](expr = Inlined(call, bindings, this.expr.asTerm).asExprOf[T])

case class EagerEntitiesPlanterExpr[T, PrepareRow: Type, Session: Type](
    uid: String,
    expr: Expr[Iterable[T]],
    fieldGetters: Expr[List[InjectableEagerPlanter[?, PrepareRow, Session]]],
    fieldClass: ast.CaseClass
)(using val tpe: Type[T], queryTpe: Type[Query[T]]) extends PlanterExpr[Query[T], PrepareRow, Session]:
  def plant(using Quotes): Expr[EagerEntitiesPlanter[T, PrepareRow, Session]] =
    val fieldClassExpr = Lifter.caseClass(fieldClass)
    '{ EagerEntitiesPlanter[T, PrepareRow, Session]($expr, ${ Expr(uid) }, $fieldGetters, $fieldClassExpr) }
  def nestInline(using Quotes)(call: Option[quotes.reflect.Tree], bindings: List[quotes.reflect.Definition]) =
    import quotes.reflect._
    this.copy[T, PrepareRow, Session](expr = Inlined(call, bindings, this.expr.asTerm).asExprOf[Iterable[T]])

object PlanterExpr {

  class Is[T: Type]:
    def unapply(expr: Expr[Any])(using Quotes) =
      import quotes.reflect._
      if (expr.asTerm.tpe <:< TypeRepr.of[T])
        Some(expr)
      else
        None

  object Uprootable {

    /** Match the generic parameters [T, PrepareRow, Session] going into InjectableEagerPlanter[T, PrepareRow, Session] */
    object MatchInjectableEager:
      def unapply(using Quotes)(term: quotes.reflect.Term) =
        import quotes.reflect._
        term match
          case Apply(TypeApply(Select(Ident("InjectableEagerPlanter"), "apply"), List(qtType, prepType, sessionType)), List(liftValue, encoder, Literal(StringConstant(uid)))) =>
            Option((qtType, prepType, sessionType, liftValue, encoder, uid))
          case _ => None

    def unapply(expr: Expr[Any])(using Quotes): Option[PlanterExpr[_, _, _]] =
      import quotes.reflect._
      // underlyingArgument application is needed on expr otherwise the InjectableEagerPlanter matchers won't work no mater how you configure them
      UntypeExpr(expr.asTerm.underlyingArgument.asExpr) match {
        case Is[EagerPlanter[_, _, _]]('{ EagerPlanter.apply[qt, prep, session]($liftValue, $encoder, ${ Expr(uid: String) }) }) =>
          Some(EagerPlanterExpr[qt, prep, session](uid, liftValue, encoder /* .asInstanceOf[Expr[GenericEncoder[A, A]]] */ ).asInstanceOf[PlanterExpr[_, _, _]])
        case Is[EagerListPlanter[_, _, _]]('{ EagerListPlanter.apply[qt, prep, session]($liftValue, $encoder, ${ Expr(uid: String) }) }) =>
          Some(EagerListPlanterExpr[qt, prep, session](uid, liftValue, encoder /* .asInstanceOf[Expr[GenericEncoder[A, A]]] */ ).asInstanceOf[PlanterExpr[_, _, _]])

        // If you uncomment this instead of '{ InjectableEagerPlanter.apply... it will also work but expr.asTerm.underlyingArgument.asExpr on top is needed
        // case Unseal(Inlined(call, defs, MatchInjectableEager(qtType, prepType, liftValue, encoder, uid))) =>
        //   (qtType.tpe.asType, prepType.tpe.asType) match
        //     case ('[qtt], '[prep]) =>
        //       encoder.tpe.asType match
        //         case '[enc] =>
        //           Some(InjectableEagerPlanterExpr[qtt, prep](uid, Inlined(call, defs, liftValue).asExpr.asInstanceOf[Expr[_ => qtt]], Inlined(call, defs, encoder).asExpr.asInstanceOf[Expr[enc & GenericEncoder[qtt, prep]]]))

        // You can't just do '{ InjectableEagerPlanter... below but also have to do this. I'm not sure why. Also you can't
        // JUST do this. You either need the clause above or the clause below otherwise it won't work
        case Unseal(MatchInjectableEager(qtType, prepType, sessionType, liftValue, encoder, uid)) =>
          (qtType.tpe.asType, prepType.tpe.asType, sessionType.tpe.asType) match
            case ('[qtt], '[prep], '[session]) =>
              encoder.tpe.asType match
                case '[enc] =>
                  Some(InjectableEagerPlanterExpr[qtt, prep, session](uid, liftValue.asExpr.asInstanceOf[Expr[_ => qtt]], encoder.asExpr.asInstanceOf[Expr[enc & GenericEncoder[qtt, prep, session]]]))

        case ('{ InjectableEagerPlanter.apply[qta, prep, session]($liftValue, $encoder, ${ Expr(uid: String) }) }) =>
          Some(InjectableEagerPlanterExpr[qta, prep, session](uid, liftValue, encoder))

        case Is[LazyPlanter[_, _, _]]('{ LazyPlanter.apply[qt, prep, session]($liftValue, ${ Expr(uid: String) }) }) =>
          Some(LazyPlanterExpr[qt, prep, session](uid, liftValue).asInstanceOf[PlanterExpr[_, _, _]])
        case Is[EagerEntitiesPlanter[_, _, _]]('{ EagerEntitiesPlanter.apply[qt, prep, session]($liftValue, ${ Expr(uid: String) }, $fieldGetters, ${ Unlifter.ast(fieldClassAst) }) }) =>
          val fieldClass =
            fieldClassAst match
              case cc: ast.CaseClass => cc
              case _ =>
                report.throwError(s"Found wrong type when unlifting liftQuery class. Expected a case class, was: ${io.getquill.util.Messages.qprint(fieldClassAst)}")
          Some(EagerEntitiesPlanterExpr[qt, prep, session](uid, liftValue, fieldGetters, fieldClass).asInstanceOf[EagerEntitiesPlanterExpr[_, _, _]])
        case other =>
          None
      }
  }

  object `(Planter).unquote` {
    def unapply(expr: Expr[Any])(using Quotes): Option[Expr[Planter[_, _, _]]] = expr match {
      case '{ ($planter: Planter[tt, pr, sess]).unquote } =>
        Some(planter /* .asInstanceOf[Expr[Planter[A, A]]] */ )
      case _ =>
        None
    }
  }

  object UprootableUnquote {
    def unapply(expr: Expr[Any])(using Quotes): Option[PlanterExpr[_, _, _]] = {
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

  def findUnquotes(expr: Expr[Any])(using Quotes): List[PlanterExpr[_, _, _]] =
    ExprAccumulate(expr, recurseWhenMatched = false) {
      // Since we are also searching expressions inside spliced quotations (in the Lifts slot) those things are not unquoted
      // so we to search all planter expressions, not just the unquotes.
      // Note however that once we have found a lift we do not need to recurse searching lifts inside of it. There is
      // one case in EagerEntitiesPlanterExpr where InjectableEagerLift is inside it but we do not care about
      // recursing into those because they are unlifted when dealing with the EagerEntitiesPlanterExpr parent
      case expr @ PlanterExpr.Uprootable(planter) => planter
    }

  // TODO Find a way to propagate PrepareRow into here
  // pull vases out of Quotation.lifts
  object UprootableList {
    def unapply(expr: Expr[List[Any]])(using Quotes): Option[List[PlanterExpr[_, _, _]]] = {
      import quotes.reflect._
      UntypeExpr(expr.asTerm.underlyingArgument.asExpr) match {
        case '{ Nil } =>
          Some(List())

        case '{ List.apply[t](${ Varargs(elems) }: _*) } =>
          val scalarValues =
            elems.map {
              case other @ PlanterExpr.Uprootable(vaseExpr) =>
                Some(vaseExpr)
              case other =>
                None
            }.collect {
              case Some(value) => value
            }

          // if all the elements match SingleValueVase then return them, otherwise don't
          if (scalarValues.length == elems.length) Some(scalarValues.toList)
          else None

        case _ =>
          None
      }
    }
  }
}

case class QuotedExpr(ast: Expr[Ast], lifts: Expr[List[Planter[_, _, _]]], runtimeQuotes: Expr[List[QuotationVase]])
object QuotedExpr {

  /** To be used internally only since it does not account for inlines that could appear in front of it */
  private object `Quoted.apply` {
    def unapply(expr: Expr[Any])(using Quotes): Option[QuotedExpr] = {
      import quotes.reflect.{Term => QTerm, _}

      expr match {
        /* No runtime lifts allowed for inline quotes so quotationPouches.length must be 0 */
        case exprr @ '{ Quoted.apply[qt]($ast, $lifts, Nil) } =>
          Some(QuotedExpr(ast, lifts, '{ Nil }))
        case TypedMatryoshka(tree) =>
          `Quoted.apply`.unapply(tree)
        case _ =>
          None
      }
    }
  }

  object UprootableWithLifts {
    def unapply(expr: Expr[Any])(using Quotes): Option[(QuotedExpr, List[PlanterExpr[_, _, _]])] =
      expr match {
        /*
        It is possible that there are inlines, if so they cannot be in the AST since that is re-synthesized on every quote call so any references they
        use have to be in the lifts/runtimeQuotes. If it is Uprootable there are no runtimeQuotes so we just have to do the nesting in the
         */
        case SealedInline(parent, defs, `Quoted.apply`(quotedExpr @ QuotedExpr(ast, PlanterExpr.UprootableList(lifts), _))) =>
          val nestInlineLifts = lifts.map(_.nestInline(parent, defs))
          Some((quotedExpr, nestInlineLifts))
        case `Quoted.apply`(quotedExpr @ QuotedExpr(ast, PlanterExpr.UprootableList(lifts), _)) =>
          Some((quotedExpr, lifts))
        case _ =>
          None
      }
  }

  def uprootableWithLiftsOpt(quoted: Expr[Any])(using Quotes): Option[(QuotedExpr, List[PlanterExpr[_, _, _]])] =
    import quotes.reflect._
    quoted match {
      case QuotedExpr.UprootableWithLifts(quotedExpr) => Some(quotedExpr)
      case _                                          =>
        // TODO Enable for trace logging
        // println("Quotations with Lifts do not meet compiletime criteria: " + Printer.TreeShortCode.show(quoted.asTerm));
        None
    }
}

sealed trait QuotationLotExpr
object QuotationLotExpr {

  def apply(expr: Expr[Any])(using Quotes): QuotationLotExpr =
    unapply(expr).getOrElse { quotes.reflect.report.throwError(s"The expression: ${expr.show} is not a valid Quoted Expression and cannot be unquoted.") }

  // Verify that a quotation is inline. It is inline if all the lifts are inline. There is no need
  // to search the AST since it has been parsed already
  def unapply(expr: Expr[Any])(using Quotes): Option[QuotationLotExpr] = {
    import quotes.reflect._
    expr match {
      case vase @ `QuotationLot.apply`(quotation, uid, rest) =>
        quotation match
          case quoted @ QuotedExpr.UprootableWithLifts(QuotedExpr(ast, _, _), lifts) =>
            // Note: If the `Quoted.apply` is inside an Inline, would we need to do the same thing that we do
            // to the lifts (i.e. nesting the Inline inside them) to the 'rest' element? I don't think so
            // because the Inline would be around `Quoted.apply` which is already inside of `QuotationLot.apply`
            // i.e. it would be QuotationLot.apply(Inline(Quoted.apply(...)), ..., rest) so I don't see how 'rest'
            // could get the contents of this Inner inline
            Some(Uprootable(uid, ast, lifts)(quoted, vase.asInstanceOf[Expr[QuotationLot[Any]]], rest))

          case _ =>
            Some(Pluckable(uid, quotation, rest))

      // If it's a QuotationLot but we can't extract it at all, need to throw an error
      case '{ ($qb: QuotationLot[t]) } =>
        Some(Pointable(qb))

      case _ =>
        None
    }
  }

  protected object `(QuotationLot).unquote` {
    def unapply(expr: Expr[Any])(using Quotes) = {
      import quotes.reflect._
      UntypeExpr(expr) match {
        // When a QuotationLot is embedded into an ast
        case '{ (${ quotationLot }: QuotationLot[tt]).unquote } =>
          Some(quotationLot)

        // TODO Now since we have UntypeExpr this below might not be needed
        // There are situations e.g. SchemaMeta where there's an additional type ascription needed
        // there it needs to be specified in the AST manually. Maybe this is a bug?
        case '{ type tt; ((${ quotationLot }: QuotationLot[`tt`]).unquote: `tt`) } =>
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
      /*
       * Specifically the inner `Uninline` part allows using metas e.g. InsertMeta that
       * are defined in parent contexts e.g.
       * class Stuff { object InsertStuff { inline given InsertMeta[Product] = insertMeta(_.id) } }
       * and then imported into other places e.g.
       * class OtherStuff extends Stuff { import InnerStuff.{given, _} } because multiple `Inline` blocks
       * will be nested around the InsertMeta.apply part.
       * It looks something like this:
       *  {
       * // If you have a look at the Term-level, this outer layer is actually one or multiple Inlined(...) parts
       * val BatchValuesSpec_this: BatchValuesJdbcSpec.this = Ex 1 - Batch Insert Normal$_this.1_<outer>
       *  ((InsertMeta[Product](Quoted[Product](Tuple(List[Ast](Property.Opinionated(Ident.Opinionated("_$V", Quat.Product("id", "description", "sku")), "id", ...))).asInstanceOf[Ast], Nil, Nil),
       *  "2e594955-b45c-4532-9bd5-ec3b3eb04138"): InsertMeta[Product]): InsertMeta[Product])
       * }
       * This will cause all manner of failure for example:
       * "The InsertMeta form is invalid. It is Pointable." Also note that this is safe to do
       * so long as we are not extracting any lifts from the Quoted.apply sections inside.
       * Otherwise, we may run into unbound-variable issues when the lifts inside the Quoted.apply are extracted.
       */
      UntypeExpr(Uninline(expr)) match {
        // Extract the entity, the uid and any other expressions the quotation bin may have
        // (e.g. the extractor if the QuotationLot is a QueryMeta). That `Uninline`
        // is needed because in some cases, the `underlyingArgument` call (that gets called somewhere before here)
        // will not be able to remove all inlines which will produce a tree that cannot be matched.
        // See https://gist.github.com/deusaquilus/29bffed4abcb8a90fccd7db61227a992#file-example-scala
        // for a example of what happens in Uninline is not here.
        case '{ Unquote.apply[t]($quotation, ${ Expr(uid: String) }) } =>
          Some((quotation, uid, List()))

        case '{ SchemaMeta.apply[t]($quotation, ${ Expr(uid: String) }) } =>
          Some((quotation, uid, List()))

        case '{ InsertMeta.apply[t]($quotation, ${ Expr(uid: String) }) } =>
          Some((quotation, uid, List()))

        case '{ UpdateMeta.apply[t]($quotation, ${ Expr(uid: String) }) } =>
          Some((quotation, uid, List()))

        case '{ CaseClassLift.apply[t]($quotation, ${ Expr(uid: String) }) } =>
          Some((quotation, uid, List()))

        case '{ QueryMeta.apply[t, r]($quotation, ${ Expr(uid: String) }, $extractor) } =>
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
      import quotes.reflect._
      unapply(expr).getOrElse {
        quotes.reflect.report.throwError(s"The expression: ${Format(Printer.TreeShortCode.show(expr.asTerm))} is not a valid unquotation of a Quoted Expression (i.e. a [quoted-expression].unqoute) and cannot be unquoted.")
      }

    def unapply(expr: Expr[Any])(using Quotes): Option[QuotationLotExpr] =
      expr match {
        // In certain situations even after doing 'underlyingArgument', there are still inline blocks
        // remaining in the AST.
        case `(QuotationLot).unquote`(QuotationLotExpr(vaseExpr)) =>
          Some(vaseExpr)
        case _ =>
          None
      }

    object Pluckable {
      def unapply(expr: Expr[Any])(using Quotes) = {
        import quotes.reflect._
        expr match {
          case vase @ `(QuotationLot).unquote`(`QuotationLot.apply`(quotation, uid, rest)) =>
            Some(uid, quotation, rest)

          case _ =>
            None
        }
      }
    }
  }

  // Not sure why this is needed by incremental compile breaks without it e.g. gives:
  // case class Pointable(expr: Expr[QuotationLot[Any]]) extends QuotationLotExpr
  import scala.quoted.Expr
  import scala.quoted.Type

  case class Pointable(expr: scala.quoted.Expr[QuotationLot[Any]]) extends QuotationLotExpr

  /**
   * QuotationLots that have runtime values hance cannot be re-planted into the scala AST and
   * they need to be put into QuotationVasees.
   * The 'other' argument is meant to be used in various unique circumstances. Right now it
   * is just used by a QueryMeta to carry an extractor function that contra-maps back to the T type
   */
  case class Pluckable(uid: String, expr: Expr[Quoted[Any]], other: List[Expr[_]]) extends QuotationLotExpr {
    def pluck(using Quotes) =
      '{ QuotationVase($expr, ${ Expr(uid) }) }
  }

  // QuotationLots expressions that can be further inlined into quotated clauses

  // Alternative more-compact representation of Uprootable, look into doing this
  // {
  //   class Uprootable(
  //     val uid: String,
  //     val ast: Expr[Ast],
  //     val inlineLifts: List[PlanterExpr[_, _, _]]
  //   )(
  //     val bin: Expr[QuotationLot[Any]],
  //     val quotation: Expr[Quoted[Any]],
  //     val rest: List[Expr[_]]
  //   )

  //   object Uprootable {
  //     def unapply(value: Uprootable): Option[(String, Expr[Ast], List[PlanterExpr[_, _, _]])] =
  //       Some((value.uid, value.ast, value.inlineLifts))

  //     object Full {
  //       def unapply(value: Uprootable): Option[(
  //         uid: String,
  //         ast: Expr[Ast],
  //         bin: Expr[QuotationLot[Any]],
  //         quotation: Expr[Quoted[Any]],
  //         inlineLifts: List[PlanterExpr[_, _, _]],
  //         rest: List[Expr[_]]
  //       )] = Some((value.uid, value.ast, value.bin, value.quotation, value.inlineLifts, value.rest))
  //     }
  //   }
  // }

  case class Uprootable(
      uid: String,
      ast: Expr[Ast],
      inlineLifts: List[PlanterExpr[_, _, _]]
  )(
      val quotation: Expr[Quoted[Any]],
      val bin: Expr[QuotationLot[Any]],
      val extra: List[Expr[_]]
  ) extends QuotationLotExpr

  object Uprootable:
    object Ast:
      def unapply(up: Uprootable): Option[Expr[Ast]] =
        Some(up.ast)
}

// This allows anyone who imports io.getquill automatically bring in QuotationLot subclasses
export QuotationLotExpr.Pointable
export QuotationLotExpr.Pluckable
export QuotationLotExpr.Uprootable
