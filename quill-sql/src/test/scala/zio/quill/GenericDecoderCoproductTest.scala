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

object StaticSealedTraitExample {
  sealed trait Shape
  object Shape {
    case class Square(width: Int, height: Int) extends Shape
    case class Circle(radius: Int) extends Shape
  }
}

class GenericDecoderCoproductTest extends Spec {
  import GenericDecoderCoproductTestAdditional._

  // // Can't find a needed reference to Type[Shape.Circle] and Type[Shape.Square] if you
  // // use sealed trait and put inside main body. Probably because scala compiler has not been able
  // to close the class yet hence it does not yet know that it is a sum

  import StaticSealedTraitExample._
  // enum Shape:
  //   case Square(width: Int, height: Int) extends Shape
  //   case Circle(radius: Int) extends Shape

  given deter: GenericRowTyper[MyResult, Shape] with {
    def apply(rr: MyResult): ClassTag[_] = {
      val typeValue = rr.get("tpe")
      typeValue match {
        case "circle" => classTag[Shape.Circle]
        case "square" => classTag[Shape.Square]
        case _        => throw new IllegalArgumentException(s"Cannot resolve type: ${typeValue})")
      }
    }
  }

  "test coproduct type" in {
    val s = MySession
    val r1 = MyResult("tpe" -> "square", "radius" -> 890, "width" -> 123, "height" -> 456)
    autoDecoder[Shape](0, r1, s) mustEqual Shape.Square(123, 456)
    val r2 = MyResult("tpe" -> "circle", "radius" -> 890, "width" -> 123, "height" -> 456)
    autoDecoder[Shape](0, r2, s) mustEqual Shape.Circle(890)
  }
}
