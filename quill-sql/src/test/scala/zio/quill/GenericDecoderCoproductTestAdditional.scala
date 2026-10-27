package zio.quill

import scala.language.implicitConversions

import zio.quill.Quoted

import zio.quill.ast._
import zio.quill.QuotationLot
import zio.quill.QuotationVase
import zio.quill.context.ExecutionType
import org.scalatest._
import zio.quill.quat.quatOf
import zio.quill.context.ExecutionType.Static
import zio.quill.context.ExecutionType.Dynamic
import zio.quill.generic.GenericDecoder
import zio.quill.generic.GenericRowTyper
import zio.quill.generic.GenericColumnResolver
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom}
import scala.collection.mutable.LinkedHashMap
import scala.reflect.ClassTag
import scala.reflect.classTag
import zio.quill.generic.DecodingType
import zio.quill.generic.GenericNullChecker

object GenericDecoderCoproductTestAdditional {
  implicit inline def autoDecoder[T]: GenericDecoder[MyResult, MySession, T, DecodingType.Generic] = ${ GenericDecoder.summon[T, MyResult, MySession] }

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

  given GenericDecoder[MyResult, MySession, String, DecodingType.Specific] with {
    def apply(index: Int, row: MyResult, session: MySession): String = row.get(index).toString
  }

  given GenericDecoder[MyResult, MySession, Int, DecodingType.Specific] with {
    def apply(index: Int, row: MyResult, session: MySession): Int = row.get(index).toString.toInt
  }

  // TODO automatically provide this in 'context'
  given res: GenericColumnResolver[MyResult] with {
    def apply(resultRow: MyResult, columnName: String): Int = {
      resultRow.resolve(columnName)
    }
  }
}
