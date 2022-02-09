package io.getquill.parser

import io.getquill.ast._

import boopickle.CompositePickler
import boopickle.Default._
import io.getquill.quat.Quat

import scala.reflect.classTag

import java.nio.ByteBuffer
import java.util.Base64
import scala.collection.mutable.LinkedHashMap

object BooSerializer:
  import QuatPicklers._
  import io.getquill.quat.{ Quat => QQuat }

  object Quat:
    def serialize(quat: QQuat): String =
      val bytes = Pickle.intoBytes(quat)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QQuat =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QQuat].fromBytes(ByteBuffer.wrap(bytes))

  object QuatProduct:
    def serialize(product: QQuat.Product): String =
      val bytes = Pickle.intoBytes(product)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QQuat.Product =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QQuat.Product].fromBytes(ByteBuffer.wrap(bytes))

end BooSerializer
