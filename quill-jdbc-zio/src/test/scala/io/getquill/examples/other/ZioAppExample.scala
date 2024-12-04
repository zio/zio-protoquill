package io.getquill.examples.other

import io.getquill._
import io.getquill.context.ZioJdbc._
import zio._
import javax.sql.DataSource

object ZioAppExample extends ZIOAppDefault {
  import ZioAppExampleServices._

  override def run =
    DataService.getPeople
      .provide(QuillContext.dataSourceLayer, DataServiceLive.layer)
      .debug("Results")
      .exitCode
}
