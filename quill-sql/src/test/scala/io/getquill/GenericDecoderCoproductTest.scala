package io.getquill

import scala.language.implicitConversions
import io.getquill.Dsl._
import io.getquill.Quoted
import io.getquill._
import io.getquill._
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
      val typeValue = rr.get("type")
      typeValue match {
        case "circle" => classTag[Shape.Circle]
        case "square" => classTag[Shape.Square]
        case _ => throw new IllegalArgumentException(s"Cannot resolve type: ${typeValue})")
      }
    }
  }

  "test coproduct type" in {
    val r1 = MyResult("type" -> "square", "radius" -> 890, "width" -> 123, "height" -> 456)
    autoDecoder[Shape](1, r1) mustEqual Shape.Square(123, 456)
    val r2 = MyResult("type" -> "circle", "radius" -> 890, "width" -> 123, "height" -> 456)
    autoDecoder[Shape](1, r2) mustEqual Shape.Circle(890)
  }
}