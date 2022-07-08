package io.getquill.examples

import io.getquill._
import io.getquill.context.qzio.ImplicitSyntax._
import io.getquill.util.LoadConfig
import zio.Console.printLine
import zio.ZIOAppDefault
import javax.sql.DataSource

object ZioAppImplicitEnv extends ZIOAppDefault {

  object Ctx extends PostgresZioJdbcContext(Literal)

  case class Person(name: String, age: Int)

  def dataSource = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource

  case class MyQueryService(ds: DataSource) {
    import Ctx._
    given Implicit[DataSource] = Implicit(ds)

    val joes = Ctx.run(query[Person].filter(p => p.name == "Joe")).implicitly
    val jills = Ctx.run(query[Person].filter(p => p.name == "Jill")).implicitly
    val alexes = Ctx.run(query[Person].filter(p => p.name == "Alex")).implicitly
    val janes = Ctx.stream(query[Person].filter(p => p.name == "Jane")).implicitly.runCollect
  }

  override def run = {
    MyQueryService(dataSource)
      .joes
      .tap(result => printLine(result.toString))
      .exitCode
  }
}
