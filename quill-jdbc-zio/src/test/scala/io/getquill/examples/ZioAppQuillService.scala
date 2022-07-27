package io.getquill.examples

import io.getquill._
import io.getquill.context.ZioJdbc._
import zio._
import zio.Console.printLine

object ZioAppQuillService extends ZIOAppDefault {
  import QuillService.Layers._
  import QuillService.Application

  override def run =
    (for {
      joes <- Application.getPeopleByName("Joe")
      _ <- printLine(joes)
      allPeople <- Application.getAllPeople()
      _ <- printLine(allPeople)
    } yield ())
    .provide(applicationLive, dataServiceLive, dataSourceLive, postgresLive).exitCode
}
