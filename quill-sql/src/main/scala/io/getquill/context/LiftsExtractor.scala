package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.LazyPlanter
import io.getquill.Planter
import io.getquill.ast.Ast

/**
 * For a query that has a filter(p => liftQuery(List("Joe","Jack")).contains(p.name)) we need to turn 
 * the "WHERE p.name in (?)" into WHERE p.name in (?, ?) i.e. to "Particularize" the query
 * to the number of elements in the query lift. In Scala2-Quill we could just access the values
 * of the liftQuery list directly since the lift was an 'Any' value directly in the AST.
 * In Scala 3 however, we need to treat the lifted list as an Expr and create an Expr[String]
 * that represents the Query that is to be during runtime based on the content of the list
 * which has to be manipulated inside of a '{ ... } block.
 */
object Particularize {
  import io.getquill.ast.{ Map => AMap, _ }
  import io.getquill.util.Interleave
  import io.getquill.idiom.StatementInterpolator._
  import scala.annotation.tailrec
  import io.getquill.idiom._
  import scala.quoted._

  def apply[PrepareRowTemp: Type](query: Unparticular.Query, lifts: List[Expr[Planter[_, _]]], runtimeLiftingPlaceholder: Expr[Int => String])(using Quotes): Expr[String] = {
    import quotes.reflect._

    enum LiftChoice:
      case ListLift(valie: EagerListPlanterExpr[Any, PrepareRowTemp])
      case SingleLift(valie: EagerPlanterExpr[Any, PrepareRowTemp])

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

    token2Expr(query.realQuery)
  }
}

object LiftsExtractor {
  def withLazy[PrepareRowTemp](allLifts: List[Planter[_, _]], row: PrepareRowTemp) =
    val lifts = allLifts.map {
      case e: EagerPlanter[_, _] => e
      case l: LazyPlanter[_, _] => 
        throw new IllegalArgumentException(s"The value ${l.value} has a lazy lift which was spliced into a Dynamic Query. Lazy Lifts are only allowed for Compile-Time queries.")
    }
    apply(lifts, row)

  def apply[PrepareRowTemp](lifts: List[Planter[_, _]], row: PrepareRowTemp) = {

    def encodeSingleElement(lift: EagerPlanter[_, _], idx: Int): (Int, PrepareRowTemp, Any) =
      val prep = lift.asInstanceOf[EagerPlanter[Any, PrepareRowTemp]].encoder(idx, lift.value, row).asInstanceOf[PrepareRowTemp]
      (1, prep, lift.value)

    // Since we have already expanded list-lifts into separate question marks in the Particularizer, now we
    // just need to individually encode the elements and set them on the PrepareRow
    // I.e. since the Pluraizer changes filter(p => liftQuery(List("Joe", "Jack").contains(p.name)))
    // to WHERE p.name IN (?, ?) we just need to write the content of the (?, ?) into the ResultRow
    // since the number of Question marks is already expanded (i.e. from the Unparticular.Query where it's just
    // one for the IN clause "WHERE p.name IN (?)" to the particular query where it's the number of elements
    // in the list i.e. "WHERE p.name IN (?, ?)")
    def encodeElementList(lift: EagerListPlanter[_, _], idx: Int): (Int, PrepareRowTemp, Any) =
      val listPlanter = lift.asInstanceOf[EagerListPlanter[Any, PrepareRowTemp]]
      val prep =
        listPlanter.values.foldLeft(row)((newRow, value) => 
          listPlanter.encoder(idx, value, newRow).asInstanceOf[PrepareRowTemp]
        )
      (listPlanter.values.length, prep, lift.values)

    // start with (0, List(), PrepareRow) and List( a: Planter(encoder("foo")), b: Planter(encoder("bar")) )
    // You get: a.encoder(0, a.value [i.e. "foo"], row) -> (0, "foo" :: Nil, row)
    // Then:    b.encoder(1, b.value [i.e. "bar"], row) -> (0, "bar" :: "foo" :: Nil, row)
    // etc...
    val (_, values, prepare) =
      lifts.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val (increment, newRow, value) =
            lift match
              case eager: EagerPlanter[_, _] => encodeSingleElement(eager, idx)
              case eagerList: EagerListPlanter[_, _] => encodeElementList(eagerList, idx)
              case _ => 
                throw new IllegalArgumentException(s"Lifts must be extracted from EagerLift or EagerList Lift but ${lift} found")
            
          (idx + 1, value :: values, newRow)
      }
    (values, prepare)
  }
}
