package io.getquill.generic

object TupleMemberUse {
  TupleMember[(1, 2)]("_1") // // // //

  case class Person(name: String, age: Int)
  TupleMember[Person]("name")
}
