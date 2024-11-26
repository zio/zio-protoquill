package io.getquill

import io.getquill.generic.{DecodingType, GenericDecoder, GenericEncoder}

extension [From, To](mapped: MappedEncoding[From, To]) {
  // Recall that foo.contramap(bar) is like foo.from(bar)
  def toEncoder[PrepareRow, Session](implicit encoder: GenericEncoder[To, PrepareRow, Session]): GenericEncoder[From, PrepareRow, Session] =
    encoder.contramap { (from: From) => mapped.f(from) }
  // Recall that foo.map(bar) is like foo.to(bar)
  def toDecoder[ResultRow, Session](implicit decoder: GenericDecoder[ResultRow, Session, From, DecodingType.Leaf]): GenericDecoder[ResultRow, Session, To, DecodingType.Leaf] =
    decoder.map { (from: From) => mapped.f(from) }

  def toSeqEncoder[PrepareRow, Session](implicit encoder: GenericEncoder[Seq[To], PrepareRow, Session]): GenericEncoder[Seq[From], PrepareRow, Session] =
    encoder.contramap { (from: Seq[From]) => from.map(mapped.f) }
  def toSeqDecoder[ResultRow, Session](implicit decoder: GenericDecoder[ResultRow, Session, Seq[From], DecodingType.Leaf]): GenericDecoder[ResultRow, Session, Seq[To], DecodingType.Leaf] =
    decoder.map { (from: Seq[From]) => from.map(mapped.f) }
}
