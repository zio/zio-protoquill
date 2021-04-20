package io.getquill.generic

object UseAnyValEncoding {
  class Foo(val value: String) extends AnyVal
  val foo = new Foo("blah")
 
  def main(args: Array[String]): Unit = {
    println( AnyValToValMacro[Foo, String].f(foo) )
    println( ValToAnyValMacro[String, Foo].f("blah") )
  }
}