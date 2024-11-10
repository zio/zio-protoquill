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
import io.getquill.generic.GenericRowTyper
import io.getquill.generic.GenericColumnResolver

import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{constValue, erasedValue, summonFrom}
import scala.collection.mutable.LinkedHashMap
import scala.reflect.ClassTag
import scala.reflect.classTag
import io.getquill.context.mirror.Row
import io.getquill.quote
import io.getquill.query
import io.getquill.context.mirror.MirrorSession

class GenericDecoderTest extends Spec {
  import StaticEnumExample._

  val ctx = new MirrorContext[MirrorSqlDialect, Literal](MirrorSqlDialect, Literal) with MirrorColumnResolving[MirrorSqlDialect, Literal]
  import ctx.{given, _}

  case class Person(name: String, age: Int)

  "domain-model product using row-typer" - {
    given RowTyper[Shape] with {
      def apply(row: Row) =
        row.apply[String]("type") match {
          case "square" => classTag[Shape.Square]
          case "circle" => classTag[Shape.Circle]
        }
    }

    "test product type" in {
      val s = MirrorSession.default
      inline def q = quote { query[Shape].filter(s => s.id == 18) }
      val result = ctx.run(q)

      val squareRow = Row("type" -> "square", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
      result.extractor(squareRow, s) mustEqual Shape.Square(18, 123, 456)
      val circleRow = Row("type" -> "circle", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
      result.extractor(circleRow, s) mustEqual Shape.Circle(18, 890)
    }
  }

  "simple examples" - {
    val s = MirrorSession.default

    "test tuple type" in {
      inline def q = quote { query[Person].map(p => (p.name, p.age)) }
      val result = ctx.run(q)

      val tupleRow = Row("_1" -> "Joe", "_2" -> 123)
      result.extractor(tupleRow, s) mustEqual ("Joe", 123)
    }

    "test case class type" in {
      inline def q = quote { query[Person] }
      val result = ctx.run(q)

      val tupleRow = Row("name" -> "Joe", "age" -> 123)
      result.extractor(tupleRow, s) mustEqual Person("Joe", 123)
    }
  }
}
object StaticEnumExample {
  enum Shape(val id: Int) {
    case Square(override val id: Int, width: Int, height: Int) extends Shape(id)
    case Circle(override val id: Int, radius: Int) extends Shape(id)
  }
  object Shape {
    given MirrorContext.GenericDecoder[Shape] = MirrorContext.deriveDecoder
  }
}
