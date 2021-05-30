package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.LazyPlanterExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.LazyPlanter
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.{ Map => AMap, _ }
import io.getquill.util.Interleave
import io.getquill.idiom.StatementInterpolator._
import scala.annotation.tailrec
import io.getquill.idiom._
import scala.quoted._
import io.getquill.util.Format

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
  object Static:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp](query: Unparticular.Query, lifts: List[Expr[Planter[_, _]]], runtimeLiftingPlaceholder: Expr[Int => String], emptySetContainsToken: Token => Token)(using Quotes): Expr[String] =
      raw(query.realQuery, lifts, runtimeLiftingPlaceholder, emptySetContainsToken)

    private[getquill] def raw[PrepareRowTemp](statement: Statement, lifts: List[Expr[Planter[_, _]]], runtimeLiftingPlaceholder: Expr[Int => String], emptySetContainsToken: Token => Token)(using Quotes): Expr[String] = {
      import quotes.reflect._

      enum LiftChoice:
        case ListLift(value: EagerListPlanterExpr[Any, PrepareRowTemp])
        case SingleLift(value: EagerPlanterExpr[Any, PrepareRowTemp])

      val listLifts: Map[String, EagerListPlanterExpr[Any, PrepareRowTemp]] =
        lifts.collect {
          case PlanterExpr.Uprootable(planterExpr: EagerListPlanterExpr[_, _]) =>
            planterExpr.asInstanceOf[EagerListPlanterExpr[Any, PrepareRowTemp]]
        }.map(lift => (lift.uid, lift)).toMap

      val singleLifts: Map[String, EagerPlanterExpr[Any, PrepareRowTemp]] =
        lifts.collect {
          case PlanterExpr.Uprootable(planterExpr: EagerPlanterExpr[_, _]) =>
            planterExpr.asInstanceOf[EagerPlanterExpr[Any, PrepareRowTemp]]
        }.map(lift => (lift.uid, lift)).toMap

      def getLifts(uid: String): LiftChoice =
        listLifts.get(uid).map(LiftChoice.ListLift(_))
          .orElse(singleLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .getOrElse {
            throw new IllegalArgumentException(s"Cannot find list-lift with UID ${uid} (from all the lifts ${lifts.map(io.getquill.util.Format.Expr(_))})")
          }

      /**
       * Actual go from a liftQuery(List("Joe", "Jack")) to "?, ?" using the lifting placeholder.
       * Also return how much the index should be incremented
       */
      def placeholders(uid: String, initialIndex: Expr[Int]): (Expr[Int], Expr[String], LiftChoice) =
        val liftType = getLifts(uid)
        liftType match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder = '{ ${lifts.expr}.zipWithIndex.map((_, index) => $runtimeLiftingPlaceholder(index + 1 + $initialIndex)).mkString(", ") }
            val liftsLength = '{ ${lifts.expr}.length }
            (liftsLength, liftsPlaceholder, liftType)
          case LiftChoice.SingleLift(lift) =>
            (Expr(1), '{ $runtimeLiftingPlaceholder($initialIndex + 1) }, liftType)


      object Matrowl:
        sealed trait Ground:
          override def toString = "Gnd"
        case object Ground extends Ground
        def Bottom = Matrowl(List(), Matrowl.Ground)

      case class Matrowl private (doneWorks: List[Expr[String]], below: Matrowl | Matrowl.Ground):
        def dropIn(doneWork: Expr[String]): Matrowl =
          //println(s"Dropping: ${Format.Expr(doneWork)} into ${this.toString}")
          this.copy(doneWorks = doneWork +: this.doneWorks)
        def stack: Matrowl =
          //println(s"Stack New Matrowl ():=> ${this.toString}")
          Matrowl(List(), this)
        def pop: (List[Expr[String]], Matrowl) =
          //println(s"Pop Top Matrowl: ${this.toString}")
          below match
            case m: Matrowl => (doneWorks, m)
            case e: Matrowl.Ground => report.throwError("Tokenization error, attempted to pop a bottom-level element")
        def pop2: (List[Expr[String]], List[Expr[String]], Matrowl) =
          //println(s"Pop Two Matrowls...")
          val (one, firstBelow) = pop
          val (two, secondBelow) = firstBelow.pop
          (one, two, secondBelow)
        def isBottom: Boolean =
          below match
            case m: Matrowl => false
            case e: Matrowl.Ground => true
        def scoop: List[Expr[String]] =
          //println(s"Scoop From Matrowl: ${this.toString}")
          doneWorks
        override def toString = s"(${doneWorks.map(Format.Expr(_)).mkString(", ")}) -> ${below.toString}"
      end Matrowl


      enum Work:
        case AlreadyDone(expr: Expr[String])
        case Token(token: io.getquill.idiom.Token)
        // Stack the Matrowl
        case Stack
        // Pop the Matrowl
        case Pop2(finished: (Expr[String], Expr[String]) => Expr[String])
      object Work:
        def StackL = List(Work.Stack)

      extension (stringExprs: Seq[Expr[String]])
        def mkStringExpr = stringExprs.foldLeft(Expr(""))((concatonation, nextExpr) => '{ $concatonation + $nextExpr })


      def token2Expr(token: Token): Expr[String] = {
        @tailrec
        def apply(
          workList: List[Work],
          matrowl: Matrowl,
          placeholderCount:   Expr[Int] // I.e. the index of the '?' that is inserted in the query (that represents a lift)
        ): Expr[String] = workList match {
          case Nil =>
            if (!matrowl.isBottom)
              report.throwError("Did not get to the bottom of the stack while tokenizing")
            matrowl.scoop.reverse.mkStringExpr
          case head :: tail =>
            head match {
              case Work.Stack => apply(tail, matrowl.stack, placeholderCount)
              case Work.Pop2(finished) =>
                // we expect left := workIfListNotEmpty and right := workIfListEmpty
                // this is the logical completion of the SetContainsToken(a, op, ScalarTagToken(tag)) case
                val (left, right, restOfMatrowl) = matrowl.pop2
                val finishedExpr = finished(left.reverse.mkStringExpr, right.reverse.mkStringExpr)
                apply(tail, restOfMatrowl.dropIn(finishedExpr), placeholderCount)

              case Work.AlreadyDone(expr)       =>    apply(tail, matrowl.dropIn(expr), placeholderCount)
              case Work.Token(StringToken(s2))  =>    apply(tail, matrowl.dropIn(Expr(s2)), placeholderCount)
              case Work.Token(SetContainsToken(a, op, b @ ScalarTagToken(tag))) =>
                val (liftsLength, liftsExpr, liftChoice) = placeholders(tag.uid, placeholderCount)
                liftChoice match
                  // If it is a list that could be empty, we have to create a branch structure that will expand
                  // both variants of that using the Matrowl nested structure
                  case LiftChoice.ListLift(_) =>
                    val workIfListNotEmpty = Work.Token(stmt"$a $op (") :: Work.AlreadyDone(liftsExpr) :: Work.Token(stmt")") :: Nil
                    val workIfListEmpty = List(Work.Token(emptySetContainsToken(a)))
                    val complete =
                      (workIfListNotEmpty: Expr[String], workIfListEmpty: Expr[String]) => '{
                        if ($liftsLength != 0) $workIfListNotEmpty else $workIfListEmpty
                      }
                    val work = Work.StackL ::: workIfListEmpty ::: Work.StackL ::: workIfListNotEmpty ::: List(Work.Pop2(complete))
                    //println(s"** Push Two Variants ** - \nWork is: ${work}\nTail is: ${tail}")
                    // We can spliced liftsLength combo even if we're not splicing in the array itself (i.e. in cases)
                    // where we're splicing the empty token. That's fine since when we're splicing the empty token, the
                    // array length is zero.
                    apply(work ::: tail, matrowl, '{ $placeholderCount + $liftsLength })

                  // Otherwise it's just a regular scalar-token expansion
                  case _ =>
                    //println(s"** Push One Variant ** - \nWork is: ${stmt"$a $op ($b)"}\nTail is: ${tail}")
                    apply(Work.Token(stmt"$a $op ($b)") +: tail, matrowl, placeholderCount)

              // The next two variants cannot be a list operation now since that was handled in the
              // Work.Token(SetContainsToken(a, op, b @ ScalarTagToken(tag))) case above
              // They can be set-operations on a lift but not one that can be empty
              case Work.Token(SetContainsToken(a, op, b)) =>
                apply(Work.Token(stmt"$a $op ($b)") +: tail, matrowl, placeholderCount)
              case Work.Token(ScalarTagToken(tag))        =>
                val (liftsLength, liftsExpr, _) = placeholders(tag.uid, placeholderCount)
                apply(tail, matrowl.dropIn(liftsExpr), '{ $placeholderCount + $liftsLength })

              case Work.Token(Statement(tokens))          =>
                apply(tokens.map(Work.Token(_)) ::: tail, matrowl, placeholderCount)
              case Work.Token(_: ScalarLiftToken) =>
                throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
              case Work.Token(_: QuotationTagToken) =>
                throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
            }
        }
        apply(List(Work.Token(token)), Matrowl.Bottom, Expr(0))
      }
      token2Expr(statement)
    }
  end Static



  object Dynamic:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp](query: Unparticular.Query, lifts: List[Planter[_, _]], liftingPlaceholder: Int => String, emptySetContainsToken: Token => Token): String =
      raw(query.realQuery, lifts, liftingPlaceholder, emptySetContainsToken)

    private[getquill] def raw[PrepareRowTemp](statements: Statement, lifts: List[Planter[_, _]], liftingPlaceholder: Int => String, emptySetContainsToken: Token => Token): String = {
      enum LiftChoice:
        case ListLift(value: EagerListPlanter[Any, PrepareRowTemp])
        case SingleLift(value: EagerPlanter[Any, PrepareRowTemp])

      val listLifts = lifts.collect { case e: EagerListPlanter[_, _] => e.asInstanceOf[EagerListPlanter[Any, PrepareRowTemp]] }.map(lift => (lift.uid, lift)).toMap
      val singleLifts = lifts.collect { case e: EagerPlanter[_, _] => e.asInstanceOf[EagerPlanter[Any, PrepareRowTemp]] }.map(lift => (lift.uid, lift)).toMap

      def getLifts(uid: String): LiftChoice =
        listLifts.get(uid).map(LiftChoice.ListLift(_))
          .orElse(singleLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .getOrElse {
            throw new IllegalArgumentException(s"Cannot find list-lift with UID ${uid} (from all the lifts ${lifts})")
          }

      // TODO Also need to account for empty tokens but since we actually have a reference to the list can do that directly
      def placeholders(uid: String, initialIndex: Int): (Int, String) =
        getLifts(uid) match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder =
              lifts.values.zipWithIndex.map((_, index) => liftingPlaceholder(index + 1 + initialIndex)).mkString(", ")
            val liftsLength = lifts.values.length
            (liftsLength, liftsPlaceholder)
          case LiftChoice.SingleLift(lift) =>
            (1, liftingPlaceholder(initialIndex + 1))

      def isEmptyListLift(uid: String) =
        getLifts(uid) match
          case LiftChoice.ListLift(lifts) => lifts.values.isEmpty
          case _ => false

      def token2String(token: Token): String = {
        @tailrec
        def apply(
          workList: List[Token],
          sqlResult: Seq[String],
          placeholderIndex: Int // I.e. the index of the '?' that is inserted in the query (that represents a lift)
        ): String = workList match {
          case Nil => sqlResult.reverse.foldLeft("")((concatonation, nextExpr) => concatonation + nextExpr)
          case head :: tail =>
            head match {
              case StringToken(s2)            => apply(tail, s2 +: sqlResult, placeholderIndex)
              case SetContainsToken(a, op, b) =>
                b match
                  case ScalarTagToken(tag) if isEmptyListLift(tag.uid) =>
                    apply(emptySetContainsToken(a) +: tail, sqlResult, placeholderIndex)
                  case _ =>
                    apply(stmt"$a $op ($b)" +: tail, sqlResult, placeholderIndex)
              case ScalarTagToken(tag)        =>
                val (liftsLength, lifts) = placeholders(tag.uid, placeholderIndex)
                apply(tail, lifts +: sqlResult, placeholderIndex + liftsLength)
              case Statement(tokens)          => apply(tokens.foldRight(tail)(_ +: _), sqlResult, placeholderIndex)
              case _: ScalarLiftToken =>
                throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
              case _: QuotationTagToken =>
                throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
            }
        }
        apply(List(token), Seq(), 0)
      }

      token2String(statements)
    }
  end Dynamic


end Particularize