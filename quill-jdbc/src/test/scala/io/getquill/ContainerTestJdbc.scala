package io.getquill

import io.getquill.context.jdbc.postgres.testContext
import io.getquill.context.sql.SqlContext
import io.getquill.generic.ArrayEncoding

trait ParentContext {
  val ctx: SqlContext[_, _] with ArrayEncoding
  import ctx._
  //def method = (i: Int) => ???.asInstanceOf[Iterable[Int]]
  //inline def c = InjectableEagerPlanter(method, arrayIntEncoder, "123")

  inline def cc = Container[Iterable[Int], ctx.PrepareRow](arrayIntEncoder)
}

object ContainerTestJdbc extends ParentContext {
  val ctx: testContext.type = testContext

  def main(args: Array[String]): Unit = {
    //val ctx = new PostgresJdbcContext[Literal](Literal, "testPostgresDB")

    import ctx._
    Macro.matchContainer(cc)

  }
}