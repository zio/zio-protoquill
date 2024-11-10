package io.getquill

import io.getquill.generic.DecodingType

trait ProductDecoders[ResultRow, Session] {
  type GenericDecoder[T] = io.getquill.generic.GenericDecoder[ResultRow, Session, T, DecodingType.Generic]
  inline def deriveDecoder[T]: GenericDecoder[T] = ${ io.getquill.generic.GenericDecoder.summon[T, ResultRow, Session] }

  private type SomeDec[T] = io.getquill.generic.GenericDecoder[ResultRow, Session, T, _]

  implicit inline def emptyTupleDecoder: GenericDecoder[EmptyTuple] = deriveDecoder
  implicit inline def tuple1Decoder[T1: SomeDec]: GenericDecoder[Tuple1[T1]] = deriveDecoder
  implicit inline def tuple2Decoder[T1: SomeDec, T2: SomeDec]: GenericDecoder[(T1, T2)] = deriveDecoder
  implicit inline def tuple3Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec]: GenericDecoder[(T1, T2, T3)] = deriveDecoder
  implicit inline def tuple4Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec]: GenericDecoder[(T1, T2, T3, T4)] = deriveDecoder
}
