package zio.quill.context.cassandra.zio.examples

import _root_.zio.quill.*
import _root_.zio.quill.cassandrazio.Quill
import _root_.zio.Console.printLine
import _root_.zio.{ZIO, ZIOAppDefault, ZLayer}

object IdiomaticApp extends ZIOAppDefault {
  import IdiomaticAppData.*

  override def run = {
    (for {
      people <- DataService.getPeople()
      _ <- printLine(s"People: ${people}")
      joes <- DataService.getPeopleByName("Joe")
      _ <- printLine(s"Joes: ${joes}")
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
}
