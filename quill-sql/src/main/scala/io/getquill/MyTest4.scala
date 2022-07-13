package io.getquill

object MyTest4 extends scala.App {

  final case class Wrapped[T](value: T)

  trait Unwrappable[In] {
    type Out
    def unwrap(value: In): Out
  }

  object Unwrappable extends UnwrappableLowPriority {

    type WithOut[In, Out0] = Unwrappable[In] { type Out = Out0 }

    def apply[In](implicit ev: Unwrappable[In]): Unwrappable[In] = ev

    implicit def wrappedIsUnwrappable[T]: Unwrappable.WithOut[Wrapped[T], T] =
      new Unwrappable[Wrapped[T]] {
        type Out = T
        def unwrap(value: Wrapped[T]): T =
          value.value
      }
  }

  trait UnwrappableLowPriority {

    implicit def everyIsUnwrappable[T]: Unwrappable.WithOut[T, T] =
      new Unwrappable[T] {
        type Out = T
        def unwrap(value: T): T =
          value
      }
  }

  trait Wrappable[In] {
    type Out
    def wrap(value: In): Out
  }

  object Wrappable extends WrappableLowPriority {

    type WithOut[In, Out0] = Wrappable[In] { type Out = Out0 }

    implicit def wrappedIsWrappable[T]: Wrappable.WithOut[Wrapped[T], Wrapped[T]] =
      new Wrappable[Wrapped[T]] {
        type Out = Wrapped[T]
        def wrap(value: Wrapped[T]): Wrapped[T] =
          value
      }
  }

  trait WrappableLowPriority {

    implicit def everyIsWrappable[T]: Wrappable.WithOut[T, Wrapped[T]] =
      new Wrappable[T] {
        type Out = Wrapped[T]
        def wrap(value: T): Wrapped[T] =
          Wrapped(value)
      }
  }

  def wrap[T](value: T)(implicit ev: Wrappable[T]): ev.Out =
    ev.wrap(value)

  println(wrap(42))
  println(wrap(Wrapped(42)))

  implicit final class FunctionSyntax[A, B](private val self: A => B) {
    def apply(a: Wrapped[A]): B =
      self(a.value)
  }

  implicit final class WrappedFunctionSyntax[A, B](private val self: Wrapped[A => B]) {
    def apply[X](a: X)(implicit unwrappable: Unwrappable.WithOut[X, A]): B =
      self.value(unwrappable.unwrap(a))
  }

  val wrappedDouble: Wrapped[Int => Int] = Wrapped((x: Int) => x * 2)
  val double: Int => Int = _ * 2

  println(double(Wrapped(42)))
  println(double(42))
  println(wrappedDouble(42))
  println(wrappedDouble(Wrapped(42)))
}
