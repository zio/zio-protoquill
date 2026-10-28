package zio.quill.examples.other

import zio.quill.{ Literal, PostgresZioJdbcContext }
import zio.quill.context.ZioJdbc._
import zio.{ Runtime, Unsafe }
import zio.quill._
import zio.quill.jdbczio.Quill

object PlainApp {

  object MyPostgresContext extends PostgresZioJdbcContext(Literal)
  import MyPostgresContext._

  case class Person(name: String, age: Int)

  val zioDS = Quill.DataSource.fromPrefix("testPostgresDB")

  def main(args: Array[String]): Unit = {
    val people = quote {
      query[Person].filter(p => p.name == "Alex")
    }
    val qzio =
      MyPostgresContext.run(people)
        .tap(result => zio.ZIO.attempt(println(result.toString)))
        .provideLayer(zioDS)

    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(qzio).getOrThrow()
    }
    ()
  }
}
