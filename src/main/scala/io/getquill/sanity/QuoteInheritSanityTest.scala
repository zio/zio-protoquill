package io.getquill.sanity

@main def sanityTest() = {
  inline def foo = "one"
  inline def bar = foo
  println( CompileTimeTree.printUnderlyingArg(bar) )
}