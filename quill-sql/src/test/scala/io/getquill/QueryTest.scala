package io.getquill

import io.getquill._
import io.getquill.Dsl._
import scala.reflect.ClassTag
import scala.reflect.classTag
import io.getquill.generic.GenericColumnResolver

object QueryTest {
  case class Person(name: String, age: Int)

  // add with MirrorColumnResolving to provide the GenericColumnResolver
  val ctx = new MirrorContext[PostgresDialect, Literal](PostgresDialect, Literal) with MirrorColumnResolving[PostgresDialect, Literal]
  import ctx.{given, _} // need to import givens so that the mirror resolver will be import
  // note, maybe that's why need to do 'implicit inline def' for generic decoder i.e. since implicits are automatically imported
  // unlike givens, so should try doing ctx.{given, _} and see if that fixes it
  import io.getquill.context.mirror.Row
  import io.getquill.generic.DecodeAlternate

  enum Shape(val id: Int):
    case Square(override val id: Int, width: Int, height: Int) extends Shape(id)
    case Circle(override val id: Int, radius: Int) extends Shape(id)

  def main(args: Array[String]): Unit = {

    given RowTyper[Shape] with
      def apply(row: Row) = 
        row.apply[String]("type") match
          case "square" => classTag[Shape.Square]
          case "circle" => classTag[Shape.Circle]

    inline def q = quote { query[Shape].filter(s => s.id == 18) }
      val result = ctx.run(q)

    val squareRow = Row("type" -> "square", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
    println(result.extractor(squareRow))
    result.extractor(squareRow) == Shape.Square(18, 123, 456)
    val circleRow = Row("type" -> "circle", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
    result.extractor(circleRow) == Shape.Circle(18, 890)
    println(result.extractor(squareRow))

    // val resultRow = Row("name" -> "Joe", "age" -> 123) //hellooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    
    // // This works
    // //println( DecodeAlternate.apply[(String, Int), ResultRow](0, resultRow) )
    
    // // This does not
    // inline def q = quote { query[Person].map(p => (p.name, p.age)) }
    // val m = run(q)
    // println( m.extractor(resultRow) )

    // inline def q2 = quote { query[Person] }
    // val m2 = run(q2)
    // println( m2.extractor(resultRow) )


  }
}