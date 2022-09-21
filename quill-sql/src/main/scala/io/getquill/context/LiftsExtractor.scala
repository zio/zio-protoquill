package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.LazyPlanter
import io.getquill.Planter
import io.getquill.ast.Ast

object LiftsExtractor:
  /** For Dynamic queries, lazy lifts are not allowed. If one is encountered, fail */
  object Dynamic:
    def apply[PrepareRowTemp, Session](allLifts: List[Planter[_, _, _]], row: PrepareRowTemp, session: Session) =
      val lifts = allLifts.map {
        case e: EagerPlanter[_, _, _]     => e
        case e: EagerListPlanter[_, _, _] => e
        case l: LazyPlanter[_, _, _] =>
          throw new IllegalArgumentException(s"The value ${l.value} has a lazy lift which was spliced into a Dynamic Query. Lazy Lifts are only allowed for Compile-Time queries.")
        case other =>
          throw new IllegalStateException(s"Found an illegal lift planter ${other} during lift extraction. All injectable and lazy lifts must have been resolved at this point.")
      }
      LiftsExtractor.apply(lifts, row, session)

  def apply[PrepareRowTemp, Session](lifts: List[Planter[_, _, _]], row: PrepareRowTemp, session: Session) = {

    def encodeSingleElement(lift: EagerPlanter[_, _, _], idx: Int, row: PrepareRowTemp): (Int, PrepareRowTemp, Any) =
      val prepRow = lift.asInstanceOf[EagerPlanter[Any, PrepareRowTemp, Session]].encoder(idx, lift.value, row, session).asInstanceOf[PrepareRowTemp]
      (1, prepRow, lift.value)

    // Since we have already expanded list-lifts into separate question marks in the Particularizer, now we
    // just need to individually encode the elements and set them on the PrepareRow
    // I.e. since the Pluralizer changes filter(p => liftQuery(List("Joe", "Jack").contains(p.name)))
    // to WHERE p.name IN (?, ?) we just need to write the content of the (?, ?) into the ResultRow
    // since the number of Question marks is already expanded (i.e. from the Unparticular.Query where it's just
    // one for the IN clause "WHERE p.name IN (?)" to the particular query where it's the number of elements
    // in the list i.e. "WHERE p.name IN (?, ?)")
    def encodeElementList(lift: EagerListPlanter[_, _, _], idx: Int, row: PrepareRowTemp): (Int, PrepareRowTemp, Any) =
      val listPlanter = lift.asInstanceOf[EagerListPlanter[Any, PrepareRowTemp, Session]]
      val prepRow =
        listPlanter.values.zipWithIndex.foldLeft(row) { case (newRow, (value, listIndex)) =>
          // If we have query[Person].filter(p => p.name == lift("Joe") && liftQuery(List(33,44)).contains(p.age))
          // We will be on index 1 (since index of the 1st lift "Joe" is 0) so then we encoder 33 at index 2
          // and 44 at index 3. Everything is incremented by 1 since we had a lift before the liftQuery.
          // Then in the next line when we return we will return 2 since the liftQuery list has two values and
          // idx + increment below will bring it up to 3 when we are done with all the lifts. If there is another
          // lift afterward, that will be the index at which it will be decoded.
          listPlanter.encoder(idx + listIndex, value, newRow, session).asInstanceOf[PrepareRowTemp]
        }
      (listPlanter.values.length, prepRow, lift.values)

    // start with (0, List(), PrepareRow) and List( a: Planter(encoder("foo")), b: Planter(encoder("bar")) )
    // You get: a.encoder(0, a.value [i.e. "foo"], row) -> (0, "foo" :: Nil, row)
    // Then:    b.encoder(1, b.value [i.e. "bar"], row) -> (0, "bar" :: "foo" :: Nil, row)
    // etc...
    val (_, values, prepare) =
      lifts.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val (increment, newRow, value) =
            lift match
              case eager: EagerPlanter[_, _, _]         => encodeSingleElement(eager, idx, row)
              case eagerList: EagerListPlanter[_, _, _] => encodeElementList(eagerList, idx, row)
              case _ =>
                throw new IllegalArgumentException(s"Lifts must be extracted from EagerLift or EagerList Lift but ${lift} found")

          (idx + increment, value :: values, newRow)
      }
    (values, prepare)
  }
end LiftsExtractor
