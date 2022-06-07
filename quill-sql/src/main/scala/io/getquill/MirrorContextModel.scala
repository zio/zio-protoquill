package io.getquill

import io.getquill.idiom.Idiom
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericColumnResolver

// TODO Note needed, cleanup
sealed trait Dummy
object DummyInst extends Dummy

// TODO Dialect <: Idiom, Naming <: NamingStrategy are not actually needed so can remove them but that causes a compile-time error. Need to figure that out
// TODO Modify some major unit tests with embedded etc... to make sure this works in all cases
trait MirrorColumnResolving[Dialect <: Idiom, Naming <: NamingStrategy] { self: MirrorContextBase[Dialect, Naming] =>
  given mirrorResover: GenericColumnResolver[Row] with {
    def apply(resultRow: Row, columnName: String): Int = resultRow.indexOfKey(columnName)
  }
}
