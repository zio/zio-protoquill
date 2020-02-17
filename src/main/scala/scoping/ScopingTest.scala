package scoping

object Blah {
  @main def scopingTest() =
    val scoping = new OuterScoped()
    inline def foobar = "blah"
    def foobarBaz = "blah"
    //println(scoping.outerCaller)
    println( scoping.outerCallerInfo(foobar) ) // hello
}
