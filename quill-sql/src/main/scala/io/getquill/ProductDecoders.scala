package io.getquill

import io.getquill.generic.DecodingType

trait ProductDecoders {
  type ResultRow
  type Session

  // TODO "object CompositeDecoder" that has a .gen function?

  // TODO make private and use Decoder (i.e. a GenericDecoder instance from the EncodingDsl)
  type CompositeDecoder[T] = io.getquill.generic.GenericDecoder[ResultRow, Session, T, DecodingType.Composite]
  inline def deriveComposite[T]: CompositeDecoder[T] = ${ io.getquill.generic.GenericDecoder.summon[T, ResultRow, Session] }

  private type SomeDec[T] = io.getquill.generic.GenericDecoder[ResultRow, Session, T, _]

  implicit inline def emptyTupleDecoder: CompositeDecoder[EmptyTuple] = deriveComposite
  implicit inline def tuple1Decoder[T1: SomeDec]: CompositeDecoder[Tuple1[T1]] = deriveComposite
  implicit inline def tuple2Decoder[T1: SomeDec, T2: SomeDec]: CompositeDecoder[(T1, T2)] = deriveComposite
  implicit inline def tuple3Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec]: CompositeDecoder[(T1, T2, T3)] = deriveComposite
  implicit inline def tuple4Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec]: CompositeDecoder[(T1, T2, T3, T4)] = deriveComposite
  implicit inline def tuple5Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5)] = deriveComposite
  implicit inline def tuple6Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6)] = deriveComposite
  implicit inline def tuple7Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7)] = deriveComposite
  implicit inline def tuple8Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8)] = deriveComposite
  implicit inline def tuple9Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = deriveComposite
  implicit inline def tuple10Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = deriveComposite
  implicit inline def tuple11Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = deriveComposite
  implicit inline def tuple12Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = deriveComposite
  implicit inline def tuple13Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = deriveComposite
  implicit inline def tuple14Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = deriveComposite
  implicit inline def tuple15Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = deriveComposite
  implicit inline def tuple16Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec, T16: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = deriveComposite
  implicit inline def tuple17Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec, T16: SomeDec, T17: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = deriveComposite
  implicit inline def tuple18Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec, T16: SomeDec, T17: SomeDec, T18: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = deriveComposite
  implicit inline def tuple19Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec, T16: SomeDec, T17: SomeDec, T18: SomeDec, T19: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = deriveComposite
  implicit inline def tuple20Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec, T16: SomeDec, T17: SomeDec, T18: SomeDec, T19: SomeDec, T20: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] = deriveComposite
  implicit inline def tuple21Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec, T16: SomeDec, T17: SomeDec, T18: SomeDec, T19: SomeDec, T20: SomeDec, T21: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] = deriveComposite
  implicit inline def tuple22Decoder[T1: SomeDec, T2: SomeDec, T3: SomeDec, T4: SomeDec, T5: SomeDec, T6: SomeDec, T7: SomeDec, T8: SomeDec, T9: SomeDec, T10: SomeDec, T11: SomeDec, T12: SomeDec, T13: SomeDec, T14: SomeDec, T15: SomeDec, T16: SomeDec, T17: SomeDec, T18: SomeDec, T19: SomeDec, T20: SomeDec, T21: SomeDec, T22: SomeDec]: CompositeDecoder[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] = deriveComposite
}
