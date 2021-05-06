package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
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
    def apply[PrepareRowTemp](query: Unparticular.Query, lifts: List[Expr[Planter[_, _]]], runtimeLiftingPlaceholder: Expr[Int => String])(using Quotes): Expr[String] =
      raw(query.realQuery, lifts, runtimeLiftingPlaceholder)

    private[getquill] def raw[PrepareRowTemp](statement: Statement, lifts: List[Expr[Planter[_, _]]], runtimeLiftingPlaceholder: Expr[Int => String])(using Quotes): Expr[String] = {
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
            throw new IllegalArgumentException(s"Cannot find list-lift with UID ${uid} (from all the lifts ${lifts})")
          }

      /** 
       * Actual go from a liftQuery(List("Joe", "Jack")) to "?, ?" using the lifting placeholder.
       * Also return how much the index should be incremented
       */
      def placeholders(uid: String, initialIndex: Expr[Int]): (Expr[Int], Expr[String]) =
        val liftType = getLifts(uid)
        liftType match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder = '{ ${lifts.expr}.zipWithIndex.map((_, index) => $runtimeLiftingPlaceholder(index + 1 + $initialIndex)).mkString(", ") }
            val liftsLength = '{ ${lifts.expr}.length }
            (liftsLength, liftsPlaceholder)
          case LiftChoice.SingleLift(lift) =>
            (Expr(1), '{ $runtimeLiftingPlaceholder($initialIndex + 1) })

      def token2Expr(token: Token): Expr[String] = {
        @tailrec
        def apply(
          workList: List[Token],
          sqlResult: Seq[Expr[String]],
          placeholderIndex:   Expr[Int] // I.e. the index of the '?' that is inserted in the query (that represents a lift)
        ): Expr[String] = workList match {
          case Nil => sqlResult.reverse.foldLeft(Expr(""))((concatonation, nextExpr) => '{ $concatonation + $nextExpr })
          case head :: tail =>
            head match {
              case StringToken(s2)            => apply(tail, Expr(s2) +: sqlResult, placeholderIndex)
              case SetContainsToken(a, op, b) => apply(stmt"$a $op ($b)" +: tail, sqlResult, placeholderIndex)
              case ScalarTagToken(tag)        => 
                val (liftsLength, liftsExpr) = placeholders(tag.uid, placeholderIndex)
                apply(tail, liftsExpr +: sqlResult, '{ $placeholderIndex + $liftsLength })
              case Statement(tokens)          => apply(tokens.foldRight(tail)(_ +: _), sqlResult, placeholderIndex)
              case _: ScalarLiftToken =>
                throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
              case _: QuotationTagToken =>
                throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
            }
        }
        apply(List(token), Seq(), Expr(0))
      }
      token2Expr(statement)
    }
  end Static

  object Dynamic:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp: Type](query: Unparticular.Query, lifts: List[Planter[_, _]], liftingPlaceholder: Int => String): String =
      raw(query.realQuery, lifts, liftingPlaceholder)
    
    private[getquill] def raw[PrepareRowTemp: Type](statements: Statement, lifts: List[Planter[_, _]], liftingPlaceholder: Int => String): String = {
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

      def placeholders(uid: String, initialIndex: Int): (Int, String) =
        val liftType = getLifts(uid)
        liftType match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder = lifts.values.zipWithIndex.map((_, index) => liftingPlaceholder(index + 1 + initialIndex)).mkString(", ")
            val liftsLength = lifts.values.length
            (liftsLength, liftsPlaceholder)
          case LiftChoice.SingleLift(lift) =>
            (1, liftingPlaceholder(initialIndex + 1))

      def token2String(token: Token): String = {
        @tailrec
        def apply(
          workList: List[Token],
          sqlResult: Seq[String],
          placeholderIndex: Int // I.e. the index of the '?' that is inserted in the query (that represents a lift)
        ): String = workList match {
          case Nil => sqlResult.reverse.foldLeft(Expr(""))((concatonation, nextExpr) => '{ $concatonation + $nextExpr })
          case head :: tail =>
            head match {
              case StringToken(s2)            => apply(tail, s2 +: sqlResult, placeholderIndex)
              case SetContainsToken(a, op, b) => apply(stmt"$a $op ($b)" +: tail, sqlResult, placeholderIndex)
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