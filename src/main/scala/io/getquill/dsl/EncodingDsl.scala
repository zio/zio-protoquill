package io.getquill.dsl

import io.getquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}  


trait EncodingDsl {
  type PrepareRow
  type ResultRow
  //type Index = Int

  type Encoder[T] = GenericEncoder[T, PrepareRow]
  type Decoder[T] = GenericDecoder[ResultRow, T]
  
}
