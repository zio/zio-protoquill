package io.getquill

import io.getquill.context.sql.{TestDecoders, TestEncoders}
import io.getquill.generic.{DecodingType, GenericDecoder, GenericNullChecker}

trait SpecEncoders extends TestEntities with TestEncoders with TestDecoders {
  type SpecSession
  type SpecPrepareRow
  type SpecResultRow

  given nullChecker: GenericNullChecker[SpecResultRow, SpecSession]
  given optionDecoder[T](using d: GenericDecoder[SpecResultRow, SpecSession, T, ?]): GenericDecoder[SpecResultRow, SpecSession, Option[T], ?]
  given longDecoder: GenericDecoder[SpecResultRow, SpecSession, Long, DecodingType.Leaf]
  given intDecoder: GenericDecoder[SpecResultRow, SpecSession, Int, DecodingType.Leaf]
  given stringDecoder: GenericDecoder[SpecResultRow, SpecSession, String, DecodingType.Leaf]
  given booleanDecoder: GenericDecoder[SpecResultRow, SpecSession, Boolean, DecodingType.Leaf]
}
