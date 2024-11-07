package io.getquill

import io.getquill.idiom.Idiom as BaseIdiom
import io.getquill.context.sql.SqlContext
import io.getquill.context.mirror.{ArrayMirrorEncoding, MirrorDecoders, MirrorEncoders, MirrorSession, Row}
import io.getquill.context.AstSplicing
import io.getquill.generic.DecodingType

object SqlMirrorContext extends MirrorDecoders with MirrorEncoders with ArrayMirrorEncoding {
  override type Session = MirrorSession
  override type PrepareRow = Row
  override type ResultRow = Row
  override type NullChecker = MirrorNullChecker
  class MirrorNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: Row): Boolean = row.nullAt(index)
  }
  implicit val nullChecker: NullChecker = new MirrorNullChecker()

  type GenericDecoder[T] = io.getquill.generic.GenericDecoder[Row, MirrorSession, T, DecodingType.Generic]
  inline def deriveDecoder[T]: GenericDecoder[T] = ${ io.getquill.generic.GenericDecoder.summon[T, Row, MirrorSession] }
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
