package io.getquill.generic

// Note: Not using abstract Index parameter in ProtoQuill since it would bleed into most planters
trait GenericEncoder[T, PrepareRow, Session] extends ((Int, T, PrepareRow, Session) => PrepareRow) {
  def apply(i: Int, t: T, row: PrepareRow, session: Session): PrepareRow
}
