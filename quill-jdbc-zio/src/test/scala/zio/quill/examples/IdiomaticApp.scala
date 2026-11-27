package zio.quill.examples

import zio.quill._
import zio.quill.context.ZioJdbc._
import zio._
import zio.Console.printLine

object IdiomaticApp extends ZIOAppDefault {
  import IdiomaticAppData.Layers._
  import IdiomaticAppData.Application

  override def run =
    (for {
      joes <- Application.getPeopleByName("Joe")
      _ <- printLine(joes)
      allPeople <- Application.getAllPeople()
      _ <- printLine(allPeople)
    } yield ())
    .provide(applicationLive, dataServiceLive, dataSourceLive, postgresLive).exitCode
}
