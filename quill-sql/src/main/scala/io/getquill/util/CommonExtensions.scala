package io.getquill.util

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
  end Either

end CommonExtensions