package io.getquill

import io.getquill.idiom.Idiom
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericColumnResolver

trait MirrorColumnResolving[+Dialect <: Idiom, +Naming <: NamingStrategy] { self: MirrorContextBase[Dialect, Naming] =>
  given mirrorResolver: GenericColumnResolver[Row] with {
    def apply(resultRow: Row, columnName: String): Int = resultRow.indexOfKey(columnName)
  }
}
