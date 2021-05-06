package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.LazyPlanter
import io.getquill.Planter
import io.getquill.ast.Ast

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
