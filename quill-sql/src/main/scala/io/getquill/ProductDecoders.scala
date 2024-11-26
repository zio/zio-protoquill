package io.getquill

import io.getquill.generic.DecodingType

trait ProductDecoders[ResultRow, Session] {
  // TODO make private and use Decoder (i.e. a GenericDecoder instance from the EncodingDsl)
  type CompositeDecoder[T] = io.getquill.generic.GenericDecoder[ResultRow, Session, T, DecodingType.Composite]
  inline def deriveComposite[T]: CompositeDecoder[T] = ${ io.getquill.generic.GenericDecoder.summon[T, ResultRow, Session] }

  private type SomeDec[T] = io.getquill.generic.GenericDecoder[ResultRow, Session, T, _]

  implicit inline def emptyTupleDecoder: CompositeDecoder[EmptyTuple] = deriveComposite
  implicit inline def tuple1Decoder[T1: SomeDec]: CompositeDecoder[Tuple1[T1]] = deriveComposite
  implicit inline def tuple2Decoder[T1: SomeDec, T2: SomeDec]: CompositeDecoder[(T1, T2)] = deriveComposite
  implicit inline def tuple3Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec]: CompositeDecoder[(T1, T2, T3)] = deriveComposite
  implicit inline def tuple4Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec]: CompositeDecoder[(T1, T2, T3, T4)] = deriveComposite
}
