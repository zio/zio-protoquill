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
import zio.quill.context.mirror.Row
import zio.quill.quote
import zio.quill.query
import zio.quill.context.mirror.MirrorSession

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
}
