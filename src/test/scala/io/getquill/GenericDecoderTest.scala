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

class GenericDecoderTest extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  case class MyResult(list: LinkedHashMap[String, Any]) {
    def get(i: Int): String = list.values.toList(i-1).toString
    def get(key: String): String = list.apply(key).toString
    def resolve(key: String): Int = list.valuesIterator.toList.indexOf(key)
  }

  given GenericDecoder[MyResult, String] with
    def apply(index: Int, row: MyResult): String = row.get(index).toString

  given GenericDecoder[MyResult, Int] with
    def apply(index: Int, row: MyResult): Int = row.get(index).toString.toInt

  implicit inline def autoDecoder[T]:GenericDecoder[MyResult, T] = GenericDecoder.derived

  // "test product type" in {
  //   val result = MyResult(LinkedHashMap("name" -> "Joe", "age" -> 123))
  //   autoDecoder[Person](1, result) mustEqual Person("Joe", 123)
  // }

  // TODO automatically provide this in 'context'
  given res: ColumnResolver[MyResult] with {
    def apply(resultRow: MyResult, columnName: String): Int = resultRow.resolve(columnName)
  }

  // // Can't find a needed reference to Type[Shape.Circle] and Type[Shape.Square] if you
  // // use sealed trait and put inside main body. Probably because scala compiler has not been able 
  // to close the class yet hence it does not yet know that it is a sum

  // TODO Include 'type' here and make sure it will write it
  enum Shape:
    case Square(width: Int, height: Int) extends Shape
    case Circle(radius: Int) extends Shape

  given sq1: GenericDecoder[MyResult, Shape.Square] = GenericDecoder.derived
  given cr1: GenericDecoder[MyResult, Shape.Circle] = GenericDecoder.derived
  

  inline given deter: RowTyper[MyResult, Shape] with {
    inline def test[T](rr: MyResult): Boolean = {
      val typeValue = rr.get("type")
      inline erasedValue[T] match {
        case _: Shape.Circle => typeValue == "circle"
        case _: Shape.Square => typeValue == "square"
        case _: Any => throw new IllegalArgumentException(s"Cannot resolve type: ${typeValue} for any shape (shapeType was: ${GenericDecoder.showType[T]})")
      }
    }
  }

  "test coproduct type" in {
    val result = MyResult(LinkedHashMap("type" -> "square", "width" -> 123, "height" -> 456))  //hello
    println( autoDecoder[Shape](1, result) ) //helloooooooo
    //println( deter.apply(result) )
  }


} 