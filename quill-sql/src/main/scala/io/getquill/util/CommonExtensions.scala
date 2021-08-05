package io.getquill.util

import java.io.ByteArrayOutputStream

/** Extensions for common scala data types */
object CommonExtensions:

  object Either:
    extension [T](opt: Option[T])
      def toEitherOr[L](leftValue: L) =
        opt match
          case Some(value) => Right(value)
          case None => Left(leftValue)

    extension [L, R](either: Either[L, R])
      def mapLeft[L1](f: L => L1): Either[L1, R] =
        either.left.map(f)
      def discardLeft(f: L => Nothing): R =
        either match
          case Left(l) => f(l)
          case Right(value) => value
  end Either

  object Throwable:
    extension (t: Throwable)
      def stackTraceToString =
        val stream = new ByteArrayOutputStream()
        val writer = new java.io.BufferedWriter(new java.io.OutputStreamWriter(stream))
        t.printStackTrace(new java.io.PrintWriter(writer))
        writer.flush
        stream.toString

  object For:
    // Allows using tuple deconstruction with either: (a, b) <- Right(("foo", "bar"))
    extension [L, R](e: Either[L, R]) {
      def withFilter(pred: R => Boolean): Either[L, R] =
        e.flatMap { value =>
          // TODO: What we need here is a lazy Either.Left!!!!
          if (pred(value)) Right(value) else Left(throw new IllegalArgumentException("Could not filter an either"))
        }
    }

end CommonExtensions