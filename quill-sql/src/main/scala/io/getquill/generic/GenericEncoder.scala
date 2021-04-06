package io.getquill.generic

// TODO abstract index int generic param Index
trait GenericEncoder[T, PrepareRow] extends ((Int, T, PrepareRow) => PrepareRow) {
  def apply(i: Int, t: T, row: PrepareRow): PrepareRow
}