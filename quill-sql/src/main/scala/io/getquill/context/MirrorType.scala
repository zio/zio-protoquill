package io.getquill.context

import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline}

class MyContext {
  implicit inline def autoMirrorType[T]: MirrorType[T] = MirrorType.generic
}

trait MirrorType[T] {
  def mirrorType: String
}

object MirrorType {
  inline def decode[T]: String =
    summonFrom {
      case ev: Mirror.ProductOf[T] =>
        s"Product-${io.getquill.util.Format.TypeOf[ev.MirroredElemLabels]}"
      case m: Mirror.SumOf[T] =>
        "Sum"
    }

  inline def generic[T]: MirrorType[T] = 
    new MirrorType[T] {
      def mirrorType: String = decode[T]
    }

  extension[T](inline value: T)
    inline def mirrorType = summonFrom {
      case mt: MirrorType[T] => mt.mirrorType
      case _ => "mirror not found"
    }
}
