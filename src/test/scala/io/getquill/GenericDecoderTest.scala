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
import io.getquill.dsl.GenericRowTyper
import io.getquill.dsl.GenericColumnResolver
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom}
import scala.collection.mutable.LinkedHashMap
import scala.reflect.ClassTag
import scala.reflect.classTag
import io.getquill.context.mirror.Row

class GenericDecoderTest extends Spec {
  import StaticEnumExample._

  val ctx = new MirrorContext[MirrorSqlDialect, Literal](MirrorSqlDialect, Literal) with MirrorColumnResolving[MirrorSqlDialect, Literal]
  import ctx.{given, _}

  case class Person(name: String, age: Int)

  given RowTyper[Shape] with
    def apply(row: Row) = 
      row.apply[String]("type") match
        case "square" => classTag[Shape.Square]
        case "circle" => classTag[Shape.Circle]

  "test product type" in {
    inline def q = quote { query[Shape].filter(s => s.id == 18) }
    val result = ctx.run(q)

    val squareRow = Row("type" -> "square", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
    result.extractor(squareRow) mustEqual Shape.Square(18, 123, 456)
    val circleRow = Row("type" -> "circle", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
    result.extractor(circleRow) mustEqual Shape.Circle(18, 890)
  }
}
object StaticEnumExample {
  enum Shape(val id: Int):
    case Square(override val id: Int, width: Int, height: Int) extends Shape(id)
    case Circle(override val id: Int, radius: Int) extends Shape(id)
}