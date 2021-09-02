package io.getquill

import scala.language.implicitConversions

import io.getquill.Quoted


import io.getquill.ast._
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.generic.GenericDecoder
import io.getquill.generic.GenericRowTyper
import io.getquill.generic.GenericColumnResolver
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom}
import scala.collection.mutable.LinkedHashMap
import scala.reflect.ClassTag
import scala.reflect.classTag
import io.getquill.generic.DecodingType

object GenericDecoderCoproductTestAdditional {
  // TODO Resore this when moving back to original way of doing generic decoders
  //implicit inline def autoDecoder[T]:GenericDecoder[MyResult, T] = GenericDecoder.generic

  implicit inline def autoDecoder[T]: GenericDecoder[MyResult, MySession, T, DecodingType.Generic] = ${ GenericDecoder.summon[T, MyResult, MySession] }

  sealed trait MySession
  object MySession extends MySession

  case class MyResult(values: (String, Any)*) {
    lazy val list = LinkedHashMap[String, Any](values.toList: _*)
    def get(i: Int): String = list.values.toList(i-1).toString
    def get(key: String): String = list.apply(key).toString
    def resolve(key: String): Int = list.keysIterator.toList.indexOf(key) + 1
  }

  given GenericDecoder[MyResult, MySession, String, DecodingType.Specific] with
    def apply(index: Int, row: MyResult, session: MySession): String = row.get(index).toString

  given GenericDecoder[MyResult, MySession, Int, DecodingType.Specific] with
    def apply(index: Int, row: MyResult, session: MySession): Int = row.get(index).toString.toInt

  // TODO automatically provide this in 'context'
  given res: GenericColumnResolver[MyResult] with {
    def apply(resultRow: MyResult, columnName: String): Int = {
      resultRow.resolve(columnName)
    }
  }
}