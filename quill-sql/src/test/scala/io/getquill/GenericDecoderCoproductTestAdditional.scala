package io.getquill

import scala.language.implicitConversions
import io.getquill.Quoted
import io.getquill.ast.*
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest.*
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.generic.{DecodingType, GenericColumnResolver, GenericDecoder, GenericEncoder, GenericNullChecker, GenericRowTyper}

import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{constValue, erasedValue, summonFrom}
import scala.collection.mutable.LinkedHashMap
import scala.reflect.ClassTag
import scala.reflect.classTag

object GenericDecoderCoproductTestAdditional {
  implicit inline def autoDecoder[T]: GenericDecoder[MyResult, MySession, T, DecodingType.Composite] = ${ GenericDecoder.summon[T, MyResult, MySession] }

  sealed trait MySession {
    type BaseNullChecker = GenericNullChecker[MyResult, MySession]
    type NullChecker = MirrorNullChecker
    class MirrorNullChecker extends BaseNullChecker {
      override def apply(index: Int, row: MyResult): Boolean = row.nullAt(index)
    }
    implicit val nullChecker: NullChecker = new MirrorNullChecker()
  }
  object MySession extends MySession

  case class MyResult(values: (String, Any)*) {
    lazy val list = LinkedHashMap[String, Any](values.toList: _*)
    def nullAt(i: Int) = list.values.toList(i) == null
    def get(i: Int): String = list.values.toList(i).toString
    def get(key: String): String = list.apply(key).toString
    def resolve(key: String): Int = list.keysIterator.toList.indexOf(key)
  }

  given GenericDecoder[MyResult, MySession, String, DecodingType.Leaf] = new GenericDecoder.WithMap[MyResult, MySession, String] {
    def apply(index: Int, row: MyResult, session: MySession): String = row.get(index).toString
  }

  given GenericDecoder[MyResult, MySession, Int, DecodingType.Leaf] = new GenericDecoder.WithMap[MyResult, MySession, Int] {
    def apply(index: Int, row: MyResult, session: MySession): Int = row.get(index).toString.toInt
  }

  // TODO automatically provide this in 'context'
  given res: GenericColumnResolver[MyResult] with {
    def apply(resultRow: MyResult, columnName: String): Int = {
      resultRow.resolve(columnName)
    }
  }
}
