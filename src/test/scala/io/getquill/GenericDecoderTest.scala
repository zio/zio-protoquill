package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._
import io.getquill.quoter.Quoted
import io.getquill.quoter._
import io.getquill._
import io.getquill.ast._
import io.getquill.quoter.QuotationLot
import io.getquill.quoter.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.dsl.GenericDecoder
import io.getquill.dsl.RowTyper
import io.getquill.dsl.ColumnResolver
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom}
import scala.collection.mutable.LinkedHashMap
import scala.reflect.ClassTag
import scala.reflect.classTag

object Example {
  sealed trait Shape
  object Shape {
    case class Square(width: Int, height: Int) extends Shape
    case class Circle(radius: Int) extends Shape
  }
}

class GenericDecoderTest extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  case class MyResult(list: LinkedHashMap[String, Any]) {
    def get(i: Int): String = list.values.toList(i-1).toString
    def get(key: String): String = list.apply(key).toString
    def resolve(key: String): Int = list.keysIterator.toList.indexOf(key) + 1
  }

  given GenericDecoder[MyResult, String] with
    def apply(index: Int, row: MyResult): String = row.get(index).toString

  given GenericDecoder[MyResult, Int] with
    def apply(index: Int, row: MyResult): Int = row.get(index).toString.toInt

  

  // "test product type" in {
  //   val result = MyResult(LinkedHashMap("name" -> "Joe", "age" -> 123))
  //   autoDecoder[Person](1, result) mustEqual Person("Joe", 123)
  // }

  // TODO automatically provide this in 'context'
  given res: ColumnResolver[MyResult] with {
    def apply(resultRow: MyResult, columnName: String): Int = {
      resultRow.resolve(columnName)
    }
  }

  // // Can't find a needed reference to Type[Shape.Circle] and Type[Shape.Square] if you
  // // use sealed trait and put inside main body. Probably because scala compiler has not been able 
  // to close the class yet hence it does not yet know that it is a sum

  import Example._
  // enum Shape:
  //   case Square(width: Int, height: Int) extends Shape
  //   case Circle(radius: Int) extends Shape


  implicit inline def autoDecoder[T]:GenericDecoder[MyResult, T] = GenericDecoder.derived
  //given sq1: GenericDecoder[MyResult, Shape.Square] = GenericDecoder.derived
  //given cr1: GenericDecoder[MyResult, Shape.Circle] = GenericDecoder.derived
  

  given deter: RowTyper[MyResult, Shape] with {
    def test(rr: MyResult): ClassTag[_] = {
      val typeValue = rr.get("type")
      typeValue match {
        case "circle" => classTag[Shape.Circle]
        case "square" => classTag[Shape.Square]
        case _ => throw new IllegalArgumentException(s"Cannot resolve type: ${typeValue})")
      }
    }
  }

  "test coproduct type" in {
    val result = MyResult(LinkedHashMap("type" -> "square", "width" -> 123, "height" -> 456))  //hello
    println( autoDecoder[Shape](1, result) ) //helloooooooo
    //println( deter.apply(result) )
  }


} 