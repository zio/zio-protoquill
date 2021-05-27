package io.getquill

import io.getquill.context.sql.SqlContext
import io.getquill.generic.ArrayEncoding

trait ParentContextTest {
  val ctx: SqlContext[_, _] with ArrayEncoding
  import ctx._
  //def method = (i: Int) => ???.asInstanceOf[Iterable[Int]]
  //inline def c = InjectableEagerPlanter(method, arrayIntEncoder, "123")

  inline def cc = Container[Iterable[Int], ctx.PrepareRow](arrayIntEncoder) //hello
}

object ContainerTest extends ParentContextTest {
  val ctx: SqlMirrorContext[PostgresDialect, Literal] = new SqlMirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)

  def main(args: Array[String]): Unit = {

    import ctx._
    Macro.matchContainer(cc)

  }
}