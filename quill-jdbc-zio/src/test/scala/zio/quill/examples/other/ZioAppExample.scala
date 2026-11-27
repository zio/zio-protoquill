package zio.quill.examples.other

import zio.quill._
import zio.quill.context.ZioJdbc._
import zio._
import javax.sql.DataSource

case class Person(name: String, age: Int)

object ZioAppExample extends ZIOAppDefault {
  import ZioAppExampleServices._

  override def run =
    DataService.getPeople
      .provide(QuillContext.dataSourceLayer, DataServiceLive.layer)
      .debug("Results")
      .exitCode
}
