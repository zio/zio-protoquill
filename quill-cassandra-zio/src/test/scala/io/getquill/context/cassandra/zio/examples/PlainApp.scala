package io.getquill.context.cassandra.zio.examples

import io.getquill.{ CassandraZioContext, _ }
import zio.Runtime
import zio.Console.printLine
import zio.Unsafe

object PlainApp {

  object MyZioPostgresContext extends CassandraZioContext(Literal)
  import MyZioPostgresContext._

  case class Person(name: String, age: Int)

  val zioSession =
    CassandraZioSession.fromPrefix("testStreamDB")

  def main(args: Array[String]): Unit = {
    inline def people = quote {
      query[Person]
    }
    val czio =
      MyZioPostgresContext.run(people)
        .tap(result => printLine(result.toString))
        .provide(zioSession)

    Unsafe.unsafe {
      Runtime.default.unsafe.run(czio).getOrThrow()
    }
    ()
  }
}
