package inlinecontext

@main def testSomeContext() = {
  val mc = new MyContext[Int]
  println( mc.summonAndReturn )
}