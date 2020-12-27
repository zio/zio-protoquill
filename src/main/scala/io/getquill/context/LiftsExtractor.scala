package io.getquill.context

import io.getquill.quoter.EagerPlanter
import io.getquill.quoter.LazyPlanter
import io.getquill.quoter.Planter

object LiftsExtractor {
  def withLazy[PrepareRowTemp](allLifts: List[Planter[_, _]], row: PrepareRowTemp) =
    val lifts = allLifts.map {
      case e: EagerPlanter[_, _] => e
      case l: LazyPlanter[_, _] => 
        throw new IllegalArgumentException(s"The value ${l.value} has a lazy lift which was spliced into a Dynamic Query. Lazy Lifts are only allowed for Compile-Time queries.")
    }
    apply(lifts, row)

  def apply[PrepareRowTemp](lifts: List[EagerPlanter[_, _]], row: PrepareRowTemp) = {
    val (_, values, prepare) =
      lifts.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val newRow = 
            lift
            .asInstanceOf[EagerPlanter[Any, PrepareRowTemp]]
            .encoder(idx, lift.value, row).asInstanceOf[PrepareRowTemp] // TODO since summoned encoders are casted
          (idx + 1, lift.value :: values, newRow)
      }
    (values, prepare)
  }
}
