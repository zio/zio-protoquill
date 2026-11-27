package zio.quill.context.cassandra.zio.examples.other

import _root_.zio.quill._
import _root_.zio.Console.printLine
import _root_.zio.{Runtime, Unsafe}

object PlainApp {

  object MyZioPostgresContext extends CassandraZioContext(Literal)
  import MyZioPostgresContext.*

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

    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(czio).getOrThrow()
    }
    ()
  }
}
