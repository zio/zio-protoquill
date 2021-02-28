package io.getquill.generic

trait GenericEncoder[T, PrepareRow] {
  def apply(i: Int, t: T, row: PrepareRow):PrepareRow
}