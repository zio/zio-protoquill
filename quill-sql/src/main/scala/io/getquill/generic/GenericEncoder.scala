package io.getquill.generic

// TODO abstract index int generic param Index
trait GenericEncoder[T, PrepareRow, Session] extends ((Int, T, PrepareRow, Session) => PrepareRow) {
  def apply(i: Int, t: T, row: PrepareRow, session: Session): PrepareRow
}