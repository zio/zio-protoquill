package io.getquill.examples

import io.getquill.jdbczio.Quill
import io.getquill._
import zio._
import zio.Console.printLine
import java.sql.SQLException

object PlainAppQuillService {
  import QuillService.Layers._
  import QuillService.Application

  def main(args: Array[String]): Unit = {
    Unsafe.unsafe {
      Runtime.default.unsafe.run(
        (for {
          joes <- Application.getPeopleByName("Joe")
          _ <- printLine(joes)
          allPeople <- Application.getAllPeople()
          _ <- printLine(allPeople)
        } yield ()).provide(applicationLive, dataServiceLive, dataSourceLive, postgresServiceLive)
      ).getOrThrow()
    }
    ()
  }
}
