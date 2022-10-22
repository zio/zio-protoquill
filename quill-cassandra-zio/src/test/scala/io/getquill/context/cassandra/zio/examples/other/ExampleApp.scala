package io.getquill.context.cassandra.zio.examples.other

import io.getquill._
import zio.Console.printLine
import zio.ZIOAppDefault

object ExampleApp extends ZIOAppDefault {

  object MyZioPostgresContext extends CassandraZioContext(Literal)
  import MyZioPostgresContext.*

  case class Person(name: String, age: Int)

  val zioSessionLayer =
    CassandraZioSession.fromPrefix("testStreamDB")

  override def run = {
    inline def people = quote {
      query[Person]
    }
    MyZioPostgresContext.run(people)
      .tap(result => printLine(result.toString))
      .provide(zioSessionLayer).exitCode
  }
}
