package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.InjectableEagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.LazyPlanterExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.LazyPlanter
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.{Map => AMap, _}
import io.getquill.util.Interleave
import io.getquill.idiom.StatementInterpolator._
import scala.annotation.tailrec
import io.getquill.idiom._
import scala.quoted._
import io.getquill.util.Format
import io.getquill.metaprog.InjectableEagerPlanterExpr
import io.getquill.parser.Lifter
import io.getquill.context.QueryExecutionBatchModel.SingleEntityLifts
import zio.Chunk
import io.getquill.metaprog.TranspileConfigLiftable
import io.getquill.util.Interpolator2
import io.getquill.util.Messages.TraceType
import io.getquill.util.TraceConfig
import io.getquill.util.Interpolator

/**
 * For a query that has a filter(p => liftQuery(List("Joe","Jack")).contains(p.name)) we need to turn
 * the "WHERE p.name in (?)" into WHERE p.name in (?, ?) i.e. to "Particularize" the query
 * to the number of elements in the query lift. In Scala2-Quill we could just access the values
 * of the liftQuery list directly since the lift was an 'Any' value directly in the AST.
 * In Scala 3 however, we need to treat the lifted list as an Expr and create an Expr[String]
 * that represents the Query that is to be during runtime based on the content of the list
 * which has to be manipulated inside of a '{ ... } block.
 */
object Particularize:

  // the following should test for that: update - extra lift + scalars + liftQuery/setContains
  object Static:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp: Type](
        query: Unparticular.Query,
        lifts: List[Expr[Planter[_, _, _]]],
        runtimeLiftingPlaceholder: Expr[Int => String],
        emptySetContainsToken: Expr[Token => Token],
        valuesClauseRepeats: Expr[Int]
    )(traceConfig: TraceConfig)(using Quotes): Expr[String] =
      import quotes.reflect._
      val liftsExpr: Expr[List[Planter[?, ?, ?]]] = Expr.ofList(lifts)
      val queryExpr: Expr[Unparticular.Query] = UnparticularQueryLiftable(query)
      val traceConfigExpr = TranspileConfigLiftable(traceConfig)
      '{ Dynamic[PrepareRowTemp]($queryExpr, $liftsExpr, $runtimeLiftingPlaceholder, $emptySetContainsToken)($traceConfigExpr)._1 }
  end Static

  object Dynamic:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp](
        query: Unparticular.Query,
        lifts: List[Planter[_, _, _]],
        liftingPlaceholder: Int => String,
        emptySetContainsToken: Token => Token,
        valuesClauseRepeats: Int = 1
    )(traceConfig: TraceConfig): (String, LiftsOrderer) =
      new Dynamic(traceConfig)(query.realQuery, lifts, liftingPlaceholder, emptySetContainsToken, valuesClauseRepeats)

  private[getquill] class Dynamic[PrepareRowTemp, Session](traceConfig: TraceConfig):
    val interp = new Interpolator(TraceType.Particularization, traceConfig, 1)
    import interp._

    def apply(statements: Statement, lifts: List[Planter[_, _, _]], liftingPlaceholder: Int => String, emptySetContainsToken: Token => Token, valuesClauseRepeats: Int): (String, LiftsOrderer) = {
      enum LiftChoice:
        case ListLift(value: EagerListPlanter[Any, PrepareRowTemp, Session])
        case SingleLift(value: Planter[Any, PrepareRowTemp, Session])
        case InjectableLift(value: Planter[Any, PrepareRowTemp, Session])

      val listLifts = lifts.collect { case e: EagerListPlanter[_, _, _] => e.asInstanceOf[EagerListPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap
      val singleLifts = lifts.collect { case e: EagerPlanter[_, _, _] => e.asInstanceOf[EagerPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap
      val injectableLifts = lifts.collect { case e: InjectableEagerPlanter[_, _, _] => e.asInstanceOf[InjectableEagerPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap

      def getLifts(uid: String): LiftChoice =
        listLifts.get(uid).map(LiftChoice.ListLift(_))
          .orElse(singleLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .orElse(injectableLifts.get(uid).map(LiftChoice.InjectableLift(_)))
          .getOrElse {
            throw new IllegalArgumentException(s"Cannot find list-lift with UID ${uid} (from all the lifts ${lifts})")
          }

      // TODO Also need to account for empty tokens but since we actually have a reference to the list can do that directly
      def placeholders(uid: String, initialIndex: Int): (Int, String, LiftChoice) =
        val liftChoiceKind = getLifts(uid)
        liftChoiceKind match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder =
              lifts.values.zipWithIndex.map((_, index) => liftingPlaceholder(index + initialIndex)).mkString(", ")
            val liftsLength = lifts.values.length
            (liftsLength, liftsPlaceholder, liftChoiceKind)
          case LiftChoice.SingleLift(lift) =>
            (1, liftingPlaceholder(initialIndex), liftChoiceKind)
          case LiftChoice.InjectableLift(lift) =>
            (1, liftingPlaceholder(initialIndex), liftChoiceKind)

      def isEmptyListLift(uid: String) =
        getLifts(uid) match
          case LiftChoice.ListLift(lifts) => lifts.values.isEmpty
          case _                          => false

      trait Work
      case class Item(token: io.getquill.idiom.Token) extends Work
      case class SetValueClauseNum(num: Int) extends Work
      case class DoneValueClauseNum(num: Int, isLast: Boolean) extends Work

      def token2String(token: io.getquill.idiom.Token): (String, LiftsOrderer) = {
        trace"Tokenization for query: $token".andLog()
        @tailrec
        def apply(
            workList: Chunk[Work],
            sqlResult: Chunk[String],
            lifts: Chunk[LiftSlot],
            liftsCount: Int, // I.e. the index of the '?' that is inserted in the query (that represents a lift)
            valueClausesIndex: Int
        ): (String, LiftsOrderer) = {
          // Completed all work
          if (workList.isEmpty) {
            val query = sqlResult.foldLeft("")((concatenation, nextExpr) => concatenation + nextExpr)
            (query, LiftsOrderer(lifts.toList)(traceConfig))
          } else {
            val head = workList.head
            val tail = workList.tail
            head match {
              case Item(StringToken(s2)) => apply(tail, sqlResult :+ s2, lifts, liftsCount, valueClausesIndex)
              case Item(SetContainsToken(a, op, b)) =>
                b match
                  case ScalarTagToken(tag) if isEmptyListLift(tag.uid) =>
                    apply(Item(emptySetContainsToken(a)) +: tail, sqlResult, lifts, liftsCount, valueClausesIndex)
                  case _ =>
                    apply(Item(stmt"$a $op ($b)") +: tail, sqlResult, lifts, liftsCount, valueClausesIndex)
              case Item(ScalarTagToken(tag)) =>
                val (liftsLength, liftPlaceholders, liftChoice) = placeholders(tag.uid, liftsCount)
                val newLift =
                  liftChoice match
                    case LiftChoice.InjectableLift(_) =>
                      LiftSlot.makeNumbered(valueClausesIndex, tag)
                    case _ =>
                      trace"Making Normal Lift ${tag.uid}".andLog()
                      LiftSlot.makePlain(tag)

                apply(tail, sqlResult :+ liftPlaceholders, lifts :+ newLift, liftsCount + liftsLength, valueClausesIndex)
              case Item(ValuesClauseToken(stmt)) =>
                val repeatedClauses =
                  (0 until valuesClauseRepeats)
                    .toChunk
                    .mapWithHasNext((i, hasNext) => List(SetValueClauseNum(i), Item(stmt), DoneValueClauseNum(i, !hasNext)))
                    .flatten

                trace"Instructions for related clauses: ${repeatedClauses}".andLog()
                apply(repeatedClauses ++ tail, sqlResult, lifts, liftsCount, valueClausesIndex)
              case Item(Statement(tokens)) =>
                apply(tokens.toChunk.map(Item(_)) ++ tail, sqlResult, lifts, liftsCount, valueClausesIndex)
              case Item(_: ScalarLiftToken) =>
                throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
              case Item(_: QuotationTagToken) =>
                throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
              case SetValueClauseNum(num) =>
                trace"Setting value clause: ${num}".andLog()
                apply(tail, sqlResult, lifts, liftsCount, num)
              case DoneValueClauseNum(num, isLast) =>
                trace"Finished value clause: ${num}".andLog()
                val reaminingWork =
                  if (!isLast)
                    Item(stmt", ") +: tail
                  else
                    tail
                apply(reaminingWork, sqlResult, lifts, liftsCount, num)
            }
          }
        }
        apply(Chunk.single(Item(token)), Chunk.empty, Chunk.empty, 0, 0)
      }

      token2String(statements)
    }
  end Dynamic

  private implicit class IterableExtensions[A](list: Iterable[A]) extends AnyVal {
    def toChunk[A] = Chunk.fromIterable(list)
  }
  private implicit class ChunkExtensions[A](val as: Chunk[A]) extends AnyVal {
    def mapWithHasNext[B](f: (A, Boolean) => B): Chunk[B] = {
      val b = Chunk.newBuilder[B]
      val it = as.iterator
      if (it.hasNext) {
        b += f(it.next(), it.hasNext)
        while (it.hasNext) {
          b += f(it.next(), it.hasNext)
        }
      }
      b.result()
    }
  }

  case class LiftSlot(rank: LiftSlot.Rank, external: ScalarTag)
  object LiftSlot {
    enum Rank:
      case Numbered(num: Int) // for values-clauses
      case Universal // for regular lifts
    def makePlain(lift: ScalarTag) = LiftSlot(Rank.Universal, lift)
    def makeNumbered(number: Int, lift: ScalarTag) = LiftSlot(Rank.Numbered(number), lift)
    object Numbered:
      def unapply(liftSlot: LiftSlot) =
        liftSlot match
          case LiftSlot(Rank.Numbered(num), ScalarTag(uid, _)) => Some((num, uid))
          case _                                               => None
    object Plain:
      def unapply(liftSlot: LiftSlot) =
        liftSlot match
          case LiftSlot(Rank.Universal, ScalarTag(uid, _)) => Some((uid))
          case _                                           => None
  }

  case class LiftsOrderer(slots: List[LiftSlot])(traceConfig: TraceConfig) {
    val interp = new Interpolator(TraceType.Particularization, traceConfig, 1)
    import interp._

    case class ValueLiftKey(i: Int, uid: String)
    def orderLifts(valueClauseLifts: List[SingleEntityLifts], regularLifts: List[Planter[?, ?, ?]]) = {
      val valueClauseLiftIndexes =
        valueClauseLifts
          .zipWithIndex
          .flatMap((entity, i) =>
            entity.lifts.map(lift => ValueLiftKey(i, lift.uid) -> lift)
          )
          .toMap
      val regularLiftIndexes =
        regularLifts.map(lift => (lift.uid, lift)).toMap

      trace"Organizing into Lift Slots: ${slots}".andLog()
      slots.map {
        case LiftSlot.Numbered(valueClauseNum, uid) =>
          valueClauseLiftIndexes
            .get(ValueLiftKey(valueClauseNum, uid))
            .getOrElse {
              throw new IllegalStateException(s"Could not find the Value-Clause lift index:${valueClauseNum},uid:${uid}. Existing values are: ${valueClauseLiftIndexes}")
            }
        case LiftSlot.Plain(uid) =>
          regularLiftIndexes
            .get(uid)
            .getOrElse {
              throw new IllegalStateException(s"Could not find the lift uid:${uid},uid:${uid}. Existing values are: ${regularLiftIndexes}")
            }
        case other =>
          throw new IllegalStateException(s"Illegal LiftSlot: ${other}")
      }
    }
  }

  private[getquill] object UnparticularQueryLiftable:
    def apply(token: Unparticular.Query)(using Quotes) = liftableUnparticularQuery(token)
    extension [T](t: T)(using ToExpr[T], Quotes) def expr: Expr[T] = Expr(t)
    import io.getquill.parser.BasicLiftable

    given liftableUnparticularQuery: BasicLiftable[Unparticular.Query] with
      def lift =
        case Unparticular.Query(basicQuery: String, realQuery: Statement) =>
          '{ Unparticular.Query(${ basicQuery.expr }, ${ StatementLiftable(realQuery) }) }
  end UnparticularQueryLiftable

  private[getquill] object StatementLiftable:
    def apply(token: Statement)(using Quotes) = liftableStatement(token)
    extension [T](t: T)(using ToExpr[T], Quotes) def expr: Expr[T] = Expr(t)
    import io.getquill.parser.BasicLiftable

    given liftableToken: BasicLiftable[Token] with
      def lift =
        // Note strange errors about SerializeHelper.fromSerialized types can happen here if NotSerializing is not true.
        // Anyway we do not want tag-serialization here for the sake of simplicity for the tokenization which happens at runtime.
        // AST serialization is generally used to make unlifting deeply nested ASTs simpler but Quotation/Scalar Tags are only 1-level deep.
        case ScalarTagToken(lift: ScalarTag)       => '{ io.getquill.idiom.ScalarTagToken(${ Lifter.NotSerializing.scalarTag(lift) }) }
        case QuotationTagToken(lift: QuotationTag) => '{ io.getquill.idiom.QuotationTagToken(${ Lifter.NotSerializing.quotationTag(lift) }) }
        case StringToken(string)                   => '{ io.getquill.idiom.StringToken(${ string.expr }) }
        case s: Statement                          => liftableStatement(s)
        case SetContainsToken(a, op, b)            => '{ io.getquill.idiom.SetContainsToken(${ a.expr }, ${ op.expr }, ${ b.expr }) }
        case ScalarLiftToken(lift)                 => quotes.reflect.report.throwError("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
        case ValuesClauseToken(stmt)               => '{ io.getquill.idiom.ValuesClauseToken(${ stmt.expr }) }

    given liftableStatement: BasicLiftable[Statement] with
      def lift =
        case Statement(tokens) => '{ io.getquill.idiom.Statement(${ tokens.expr }) }
  end StatementLiftable
end Particularize
