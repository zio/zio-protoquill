package io.getquill.context

import io.getquill.EagerPlanter
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

  def apply[PrepareRowTemp](lifts: List[EagerPlanter[_, _]], row: PrepareRowTemp) = {
    // start with (0, List(), PrepareRow) and List( a: Planter(encoder("foo")), b: Planter(encoder("bar")) )
    // You get: a.encoder(0, a.value [i.e. "foo"], row) -> (0, "foo" :: Nil, row)
    // Then:    b.encoder(1, b.value [i.e. "bar"], row) -> (0, "bar" :: "foo" :: Nil, row)
    // etc...
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
