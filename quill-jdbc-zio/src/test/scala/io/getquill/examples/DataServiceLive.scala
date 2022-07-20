package io.getquill.examples

import io.getquill._
import io.getquill.jdbczio.Quill
import io.getquill.context.ZioJdbc._
import zio._
import javax.sql.DataSource

/**
 * Not sure why but if you put this into the same class as the caller (e.g. ZioAppExample)
 * then dotty will throw:
 * {{
 *   DataService.getPeople.provide(Ctx.dataSourceLayer, (DataServiceLive.apply(_)).toLayer)
 * }}
 */
object ZioAppExampleServices {
  object QuillContext extends PostgresZioJdbcContext(SnakeCase) {
    val dataSourceLayer = Quill.DataSource.fromPrefix("testPostgresDB").orDie
  }

  final case class DataServiceLive(dataSource: DataSource) {
    import QuillContext._
    def getPeople = run(query[Person]).provideEnvironment(ZEnvironment(dataSource))
    def getPeopleOlderThan(age: Int) = run(query[Person].filter(p => p.age > lift(age))).provideEnvironment(ZEnvironment(dataSource))
  }

  object DataService {
    def getPeople =
      ZIO.serviceWith[DataServiceLive](_.getPeople)
    def getPeopleOlderThan(age: Int) =
      ZIO.serviceWith[DataServiceLive](_.getPeopleOlderThan(age))
  }

  object DataServiceLive {
    val layer = ZLayer.fromFunction(DataServiceLive.apply)
  }
}