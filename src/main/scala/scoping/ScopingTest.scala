package scoping

@main def scopingTest() =
  val scoping = new OuterScoped()
  //println(scoping.outerCaller)
  println( scoping.outerCallerInfo(scoping.outerCaller) )
