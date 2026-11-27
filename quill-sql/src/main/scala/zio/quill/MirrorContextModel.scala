package zio.quill

import zio.quill.idiom.Idiom
import zio.quill.context.mirror.Row
import zio.quill.generic.GenericColumnResolver

trait MirrorColumnResolving[+Dialect <: Idiom, +Naming <: NamingStrategy] { self: MirrorContextBase[Dialect, Naming] =>
  given mirrorResover: GenericColumnResolver[Row] with {
    def apply(resultRow: Row, columnName: String): Int = resultRow.indexOfKey(columnName)
  }
}
