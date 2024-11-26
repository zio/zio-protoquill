package io.getquill

import io.getquill.idiom.Idiom as BaseIdiom
import io.getquill.context.sql.SqlContext
import io.getquill.context.mirror.{ArrayMirrorEncoding, MirrorDecoders, MirrorEncoders, MirrorSession, Row}
import io.getquill.context.AstSplicing

object SqlMirrorContext extends ProductDecoders[Row, MirrorSession] with MirrorDecoders with MirrorEncoders with ArrayMirrorEncoding {
  override type Session = MirrorSession
  override type PrepareRow = Row
  override type ResultRow = Row
  class MirrorNullChecker extends NullChecker {
    override def apply(index: Int, row: Row): Boolean = row.nullAt(index)
  }
  implicit val nullChecker: NullChecker = new MirrorNullChecker()
}

/** Workaround for IntelliJ SCL-20185. Inheriting MirrorContextBase directly so that `run` methods have autocomplete. */
class SqlMirrorContext[+Idiom <: BaseIdiom, +Naming <: NamingStrategy](val idiom: Idiom, val naming: Naming)
    extends MirrorContextBase[Idiom, Naming]
    with AstSplicing
    with SqlContext[Idiom, Naming] {

  export SqlMirrorContext.{
    PrepareRow => _,
    ResultRow => _,
    Session => _,
    _
  }

  val session: MirrorSession = MirrorSession("DefaultMirrorContextSession")
} // end SqlMirrorContext
