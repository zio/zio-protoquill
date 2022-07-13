package io.getquill

object MyTest3 {

  final case class Wrapped[T](value: T)

  trait Unwrappable[In] {
    type Out
    inline def unwrap(inline value: In): Out
  }

  object Unwrappable extends UnwrappableLowPriority {
    trait WithOut[In, Out0] extends Unwrappable[In] { type Out = Out0 }
    inline def apply[In](using inline ev: Unwrappable[In]): Unwrappable[In] = ev
    inline given wrappedIsUnwrappable[T]: WithOut[Wrapped[T], T] with
      type Out = T
      inline def unwrap(inline value: Wrapped[T]): T = value.value
  }

  trait UnwrappableLowPriority {
    given everyIsUnwrappable[T]: Unwrappable.WithOut[T, T] with
      type Out = T
      inline def unwrap(inline value: T): T = value
  }

  trait Wrappable[In] {
    type Out
    inline def wrap(inline value: In): Out
  }

  object Wrappable extends WrappableLowPriority {
    trait WithOut[In, Out0] extends Wrappable[In] { type Out = Out0 }
    inline given wrappedIsWrappable[T]: Wrappable.WithOut[Wrapped[T], Wrapped[T]] with
      type Out = Wrapped[T]
      inline def wrap(inline value: Wrapped[T]): Wrapped[T] =
        value
  }

  trait WrappableLowPriority {

    inline given everyIsWrappable[T]: Wrappable.WithOut[T, Wrapped[T]] with
      type Out = Wrapped[T]
      inline def wrap(inline value: T): Wrapped[T] =
        Wrapped(value)
  }

  inline def wrap[T](inline value: T)(using ev: Wrappable[T]): ev.Out =
    ev.wrap(value)

  println(wrap(42))
  println(wrap(Wrapped(42)))

  extension [A, B](inline self: A => B) {
    inline def apply(inline a: Wrapped[A]): B = self(a.value)
  }

  extension [A, B](inline self: Wrapped[A => B]) {
    inline def apply[X](a: X)(using inline unwrappable: Unwrappable.WithOut[X, A]): B =
      self.value(unwrappable.unwrap(a))
  }

  val wrappedDouble: Wrapped[Int => Int] = Wrapped((x: Int) => x * 2)
  val double: Int => Int = _ * 2

  def main(args: Array[String]): Unit = {
    println(double(Wrapped(42)))
    println(double(42))
    println(wrappedDouble(42))
    println(wrappedDouble(Wrapped(42)))
  }
}
