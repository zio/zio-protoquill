package io.getquill

object MyTest5 {

  final case class Quoted[T](value: T)

  trait Unquoteable[In]:
    type Out
    inline def unquote(inline value: In): Out

  object Unquoteable extends UnquoteableUnwrappableLowPriority:
    trait WithOut[In, Out0] extends Unquoteable[In] { type Out = Out0 }
    inline def apply[In](using inline ev: Unquoteable[In]): Unquoteable[In] = ev
    inline given quotedIsQuoteable[T]: WithOut[Quoted[T], T] with
      type Out = T
      inline def unquote(inline value: Quoted[T]): T = value.value

  trait UnquoteableUnwrappableLowPriority:
    given everyIsQuoteable[T]: Unquoteable.WithOut[T, T] with
      type Out = T
      inline def unquote(inline value: T): T = value

  trait Quoteable[In] {
    type Out
    inline def wrap(inline value: In): Out
  }

  extension [A, B](inline self: A => B) {
    inline def apply(inline a: Quoted[A]): B = self(a.value)
  }

  extension [A, B](inline self: Quoted[A => B]) {
    inline def apply[X](a: X)(using inline unwrappable: Unquoteable.WithOut[X, A]): B =
      self.value(unwrappable.unquote(a))
  }

  val wrappedDouble: Quoted[Int => Int] = Quoted((x: Int) => x * 2)
  val double: Int => Int = _ * 2

  def main(args: Array[String]): Unit = {
    println(double(Quoted(42)))
    println(double(42))
    println(wrappedDouble(42))
    println(wrappedDouble(Quoted(42)))
  }
}
