package io.getquill.context.cassandra.zio.examples

import io.getquill.*
import io.getquill.cassandrazio.Quill
import zio.Console.printLine
import zio.{ZIO, ZIOAppDefault, ZLayer}

object IdiomaticApp extends ZIOAppDefault {
  import IdiomaticAppData.*

  override def run =
    (for {
      people <- DataService.getPeople()
      _      <- printLine(s"People: ${people}")
      joes   <- DataService.getPeopleByName("Joe")
      _      <- printLine(s"Joes: ${joes}")
    } yield ())
      .provide(
        Quill.CassandraZioSession.fromPrefix("testStreamDB"),
        Quill.Cassandra.fromNamingStrategy(Literal),
        QueryService.live,
        DataService.live
      )
      .tapError(e => ZIO.succeed(println(s"Error Occurred: ${e}")))
      .exitCode
}
