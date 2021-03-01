package io.getquill.mytest

trait MyEncoder[T] {
  def encode(t: T): String
}

class MyContext {
  given intEncoder: MyEncoder[Int] = new MyEncoder[Int] { def encode(t: Int) = s"${t}" }
}
