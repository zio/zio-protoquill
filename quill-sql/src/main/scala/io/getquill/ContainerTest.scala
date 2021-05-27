package io.getquill

object ContainerTest {
  def main(args: Array[String]): Unit = {
    val ctx = new SqlMirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)
    import ctx._


    def method = (i: Int) => ???.asInstanceOf[Iterable[Int]]
    inline def c = InjectableEagerPlanter(method, arrayIntEncoder, "123")
    Macro.matchInjector(c)

  }
}